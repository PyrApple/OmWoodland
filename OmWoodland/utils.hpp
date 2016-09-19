//
//  utils.hpp
//  WoodlandProtoV5
//
//  Created by David Poirier-Quinot on 26/08/16.
//  Copyright © 2016 David Poirier-Quinot. All rights reserved.
//

#ifndef utils_hpp
#define utils_hpp

#include <stdio.h>
#include <math.h>
#include <vector>

// represents a node (cellphone) in the network
struct NetworkNode
{
    int id;
    double pos [3];
    double rot [3];
};

// return distance between two network nodes
float getDistBetweenNetworkNodes(NetworkNode n1, NetworkNode n2)
{
    return sqrtf(  powf(n1.pos[0] - n2.pos[0], 2)
                 + powf(n1.pos[1] - n2.pos[1], 2)
                 + powf(n1.pos[2] - n2.pos[2], 2) );
}

// tree node (or tree leave) is used to fill the propagation tree.
// each tree node holds a single network node, propagation parameters values,
// and a vector to store children nodes (daughter leaves)
class TreeNode
{
public:
    int id;    // node id (matches network's)
    std::vector<TreeNode> children;  // leaves
    int depth = 0; // depth of node in tree
    
    // float time;
    // float gain;
    float pathDistance;
    unsigned int maxNumberOfLeavesPerNode;
    
    TreeNode(unsigned int maxNumberOfLeavesPerNodeIn)
    {
        maxNumberOfLeavesPerNode = maxNumberOfLeavesPerNodeIn;
        // reserve some space to avoid pointer invalidated when using push_back on vector
        // (may need to replace it with an array for performance, but the push_back mecanism
        // avoids to keep counting were we are in the tree.
        children.reserve(maxNumberOfLeavesPerNode);
    };
    ~TreeNode(){};
};

// A tree represents a given propagation scenario. The emitted signal starts from its root node and
// propagates along its leaves, propagation parameters ever evolving as the signal propagates
// (e.g. time increasing, signal strength decreasing, etc.)
class Tree
{
    
public:
    
    int depth = 0; // tree depth in term of leaves level
    TreeNode root; // tree root, represents the emitter node
    
    Tree():
        root(1) // dummy root at creation (will be erased to create one with a "maxNumberOfLeavesPerNode" that matches the number of nodes in the network)
    {}
    
    ~Tree() {}
    
    // recursively print tree nodes to console
    void PrintRecursive(TreeNode* currentTreeNode)
    {
        for( int i = 0; i < currentTreeNode->children.size(); i++ )
        {
            for( int i = 0; i < currentTreeNode->depth; i++ ) std::cout << "    ";
            std::cout << "" << currentTreeNode->id << " - " ;
            std::cout << currentTreeNode->children[i].id << "\n";
            PrintRecursive(&currentTreeNode->children[i]);
        }
    }
    
};

OmAudioBuffer OmAudioBufferCreate(unsigned int numChannels, unsigned int numSamples)
{
    // create test inputs: input buffer
    OmAudioBuffer b;
    b.numChannels = numChannels;
    b.numSamples = numSamples;
    float **data; data = new float*[b.numChannels];
    for (int i = 0; i < b.numChannels; ++i) {
        data[i] = new float[b.numSamples];
    }
    b.data = data;
    return b;
}

void OmAudioBufferClear(OmAudioBuffer b)
{
    for (int i = 0; i < b.numChannels; ++i) {
        for (int j = 0; j < b.numSamples; ++j) {
            b.data[i][j] = 0.f;
        }
    }
}

void OmAudioBufferCopyAtoB(OmAudioBuffer a, OmAudioBuffer b)
{
    unsigned int numSamples = fmin(a.numSamples, b.numSamples);
    unsigned int numChannels = fmin(a.numChannels, b.numChannels);
    
    for (int i = 0; i < numChannels; ++i) {
        for (int j = 0; j < numSamples; ++j) {
            b.data[i][j] = a.data[i][j];
        }
    }
}


#endif /* utils_hpp */
