//
//  propagationHandler.hpp
//  WoodlandProtoV5
//
//  Created by David Poirier-Quinot on 26/08/16.
//  Copyright © 2016 David Poirier-Quinot. All rights reserved.
//

#ifndef propagationHandler_hpp
#define propagationHandler_hpp

#include <stdio.h>
#include <map>

#include "utils.hpp"

class PropagationHandler
{
    
// ATTRIBUTES =============================================================
    
public:

    unsigned int sampleRate = 44100; // sample rate, in Hz
    float propagationSpeed = 343;    // in m.s-1
    float propagationGain = 0.9;     // S.I. (loss per meter)
    float propagationRxGainThreshold = 0.1; // S.I.
    
private:
    
    // network holds nodes (i.e. cellphones) of the network
    std::map<int, NetworkNode> network;
    
    // irMap holds Impulse Responses, one "line" per parameters (e.g. time, gain, etc.)
    // format: [networkNodeID][propagation param ID][IR tap value]
    std::map< int,  std::map< int, std::vector< float > > > irMap;
    
    Tree propagationTree; // each leave of this tree represents a bounce on a given node of the network
    
    unsigned int MAX_TREE_DEPTH = 30; // limit tree size to avoid divergent (well, it's divergent all the same) calculation due to recursive methods
    
// METHODS ================================================================

public:
    
    PropagationHandler():propagationTree() // dummy tree to start with
    {}
    
    ~PropagationHandler() {}
    
    // loop over nodes to register them as network nodes (register node id, pos, rot, etc. in local map)
    void RegisterTopology( OmListOfList * nodesPos )
    {
        // first clear the network map, to remove old nodes from previous OpenMusic run
        network.clear();
        
        // loop over network nodes
        for( int i = 0; i < nodesPos->size1; i++ )
        {
            // create and instantiate node
            NetworkNode node;
            node.id = i + 1;
            
            // register node position
            for( int j = 0; j < nodesPos->size2; j++ )
            {
                node.pos[j] = nodesPos->data[i][j];
                node.rot[j] = 0.f;
            }
            
            // store node in local map
            network[node.id] = node;
        }
    }
    
    // print registered network topology to console
    void PrintTopology()
    {
        for(auto& item : network)
        {
            std::cout << "network node " << item.first << ": pos = ( ";
            for( int i = 0; i < 3; i++ ) std::cout << item.second.pos[i] << " ";
            std::cout << ") rot = ( ";
            for( int i = 0; i < 3; i++ ) std::cout << item.second.rot[i] << " ";
            std::cout << ")\n";
        }
    }
    
    // create main propagation tree, set its root to represent the emitter node
    // from which the signals spreads
    void CreateTree( unsigned int emitterNodeId )
    {
        // create tree root node
        int maxNumberOfLeavesPerNode = (int)network.size() - 1;
        TreeNode root( maxNumberOfLeavesPerNode );
        
        // setup root node to represent emitter
        root.id = emitterNodeId; root.depth = 0;
        // root.time = 0.0f; root.gain = 0.99f;
        root.pathDistance = 0.0f;
        
        // add root to propagation tree
        propagationTree.root = root;
    }

    // prepare IR map to receive new values
    void InitIrMap()
    {
        // clear (eventual) old values
        irMap.clear();
        
        // add first burst (emitter's)
        irMap[propagationTree.root.id][0].push_back(0.f); // init time
        irMap[propagationTree.root.id][1].push_back(1.f); // init amplitude
        irMap[propagationTree.root.id][2].push_back(1); // init pitch shifting
    }
    
    // prepare tree growth and call recursive growth
    void GrowTree()
    {
        // prepare IR map to receive new values
        InitIrMap();
        
        // propagationTree.Grow(&propagationTree.root, network, MAX_TREE_DEPTH);
        GrowTreeRecursive( &propagationTree.root, network );
    }
    
    // recursively add leaves to the propagation tree, each path from tree's root to a given leave
    // represents a propagation path (e.g. 1->2->1->3->2->etc.). Each time a leave is added to the
    // tree, an equivalent tap is added to the IR of the corresponding network node.
    void GrowTreeRecursive( TreeNode* currentTreeNode, std::map<int, NetworkNode>& network )
    {
        // loop over network nodes
        for(auto& item : network)
        {
            // continue if didn't reached max depth nor 'I emit to myself' scenario
            if( ( currentTreeNode->depth < MAX_TREE_DEPTH) && (item.first != currentTreeNode->id) )
            {
                // create new leave
                TreeNode treeNode( propagationTree.root.maxNumberOfLeavesPerNode );
                treeNode.id = item.first;
                treeNode.depth = currentTreeNode->depth + 1;
                treeNode.pathDistance = currentTreeNode->pathDistance + getDistBetweenNetworkNodes(network[currentTreeNode->id], network[treeNode.id]);

                // add a bit of randomness in bounce time (to simulate network congestion and/or avoid summing perfectly matching bursts that saturate audio output
                // treeNode.time += (rand()%100) / (propagationSpeed * 100);
                
                // check if there is still enought power in the bounce to register it
                float pathGain = pow( propagationGain, treeNode.pathDistance );
                if( pathGain > propagationRxGainThreshold)
                {
                    // compute propagation gain, time, etc. associated to the bounce that this leave represents
                    float pathTime = treeNode.pathDistance / propagationSpeed;
                    
                    // discard messages arriving at the same time than a previous one to avoid additive
                    // summation of reflection, leading to divergent amplitude in output signal
                    bool timeSlotalreadyOccupied = false;
                    for( int i = 0; i < irMap[treeNode.id][0].size(); i++ )
                    {
                        if( fabsf( irMap[treeNode.id][0][i] - pathTime ) < 1e-3 )
                        {
                            timeSlotalreadyOccupied = true;
                            // std::cout << "discarding timeslot\n";
                        }
                    }
                    
                    // register new leave only if "time slot not already occupied"
                    if( !timeSlotalreadyOccupied )
                    {
                        // add new leave to current node
                        currentTreeNode->children.push_back(treeNode);
                        propagationTree.depth = fmax(propagationTree.depth, treeNode.depth);
                        
                        
                        // fill in IR table with new data
                        irMap[treeNode.id][0].push_back(pathTime);
                        irMap[treeNode.id][1].push_back(pathGain);
                        irMap[treeNode.id][2].push_back((1.f + treeNode.depth * 0.1f)); // pitch shifting
                        
                        // grow tree from new leave
                        GrowTreeRecursive(&currentTreeNode->children.back(), network);
                    }
                }
            }
        }
    }
    
    // print tree structure to console
    void PrintTree()
    {
        propagationTree.PrintRecursive(&propagationTree.root);
    }
    
    // returns latest (time-wise) bounce time in IR
    float GetMaxPropagationTime()
    {
        float maxPropagationTime = 0.0f;
        
        for(auto& item : network)
        {
            int nodeId = item.first;
            
            // skip scenario where nodes are too far appart for initial emitter signal to reach one of them
            // even once
            if( irMap[nodeId][0].size() > 0 )
            {
                // find time of latest bounce for current node
                auto maxTimeTemp = std::max_element(std::begin(irMap[nodeId][0]), std::end(irMap[nodeId][0]));
                // std::cout << "Max duration is " << *biggest << " at position " << std::distance(std::begin(irMap[nodeId][0]), biggest) << std::endl;
                if( *maxTimeTemp > maxPropagationTime ) maxPropagationTime = *maxTimeTemp;
            }
        }
        return maxPropagationTime;
    }
    
    // print irMap content to console
    void PrintIR()
    {
        for(auto& item : irMap)
        {
            std::cout << "\nnetwork node id: " << item.first << "\n";
            for( int i = 0; i < item.second.size(); i++ )
            {
                std::cout << "[param id: " << i << "]\n";
                for( int j = 0; j < item.second[i].size(); j++ )
                {
                    std::cout << item.second[i][j] << " ";
                }
                std::cout << "\n";
            }
        }
    }
    
    // write whole bufferIn content to bufferOut, once per tap in IR of nodeId
    void ConvolveIR(OmAudioBuffer * const bufferIn, OmAudioBuffer * const bufferOut, unsigned int nodeId)
    {
        
        // clear output buffer
        OmAudioBufferClear(*bufferOut);

        // prepare working buffer
        OmAudioBuffer bufferWork;
        bufferWork = OmAudioBufferCreate(1, bufferIn->numSamples);
        
        // loop over tap in IR of current node
        for( int i = 0; i < irMap[nodeId][0].size(); i++)
        {
            // copy content of input buffer to working buffer
            OmAudioBufferCopyAtoB(*bufferIn, bufferWork);
            
            
            // apply effect: gain = f(distance)
            float tapGain = irMap[nodeId][1][i];
            for( int j = 0; j < bufferWork.numSamples; j++)
            {
                bufferWork.data[0][j] *= tapGain;
            }
            
            // apply effect: pitch = f(number of bounces in current path)
            float pitchFactor = irMap[nodeId][2][i];
            for( int j = 0; j < (int)floor(bufferWork.numSamples / pitchFactor); j++)
            {
                float fIndex = pitchFactor * j;
                
                bufferWork.data[0][j] =
                (fIndex - floor(fIndex)) * bufferWork.data[0][(int)ceil(fIndex)] +
                (1.f - (fIndex - floor(fIndex)))  * bufferWork.data[0][(int)floor(fIndex)];
                
            }
            for( int j = floor(bufferWork.numSamples / pitchFactor); j < bufferWork.numSamples; j++)
            {
                bufferWork.data[0][j] = 0.f;
            }
            
            // copy working buffer to output
            int startSample = irMap[nodeId][0][i] * sampleRate;
            for( int j = 0; j < bufferIn->numSamples; j++)
            {
                bufferOut->data[0][startSample + j] += bufferWork.data[0][j];
            }
        }
    }

};


#endif /* propagationHandler_hpp */
