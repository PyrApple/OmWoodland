//
//  main.cpp
//  test-OmWoodlandLib
//
//  Created by David Poirier-Quinot on 14/09/16.
//  Copyright © 2016 David Poirier-Quinot. All rights reserved.
//

#include <iostream>
#include <cmath>
#include "OmWoodland.hpp"
#include <stdlib.h>

OmListOfList getTopology(int numNodes){
    // create test inputs: nodes positions
    OmListOfList nodesPos;
    nodesPos.size1 = numNodes; nodesPos.size2 = 3;
    float **data; data = new float*[nodesPos.size1];
    for (int i = 0; i < nodesPos.size1; ++i) {
        data[i] = new float[nodesPos.size2];
        
        for (int j = 0; j < nodesPos.size2; ++j) {
            if( j < 2 ) data[i][j] = i*j + i + (rand() % 100) / 100.f;
            else data[i][j] = 0.f;
        }
    }
    
//    data[0][0] = -8.27; data[0][1] = 8.4; data[0][2] = 0;
//    data[1][0] = -0.25; data[1][1] = -0.24; data[1][2] = 0;
//    data[2][0] = 7.99; data[2][1] = 8.65; data[2][2] = 0;
//    data[3][0] = 7.11; data[3][1] = -8.88; data[3][2] = 0;
    
    nodesPos.data = data;
    return nodesPos;
}

OmAudioBuffer getaudioBuffer(int numChannels, int numSamples){
    
    // create test inputs: input buffer
    OmAudioBuffer bufferIn;
    bufferIn = OmAudioBufferCreate(numChannels, numSamples);
    OmAudioBufferClear(bufferIn);
    
//    bufferIn.numChannels = numChannels;
//    bufferIn.numSamples = numSamples;
//    float **data; data = new float*[bufferIn.numChannels];
//    for (int i = 0; i < bufferIn.numChannels; ++i) {
//        data[i] = new float[bufferIn.numSamples];
//        for (int j = 0; j < bufferIn.numSamples; ++j) {
//            data[i][j] = 0.f;
//        }
//    }
//    bufferIn.data = data;

    return bufferIn;
    
}

int main(int argc, const char * argv[]) {

    
    // create test inputs: misc.
    int numNodes = 3;
    float speed = 10;
    float gain = 0.9;
    float rxThreshold = 0.1;
    int sampleRate = 44100;
    float numSamplesIn = 300;
    
    OmListOfList nodesPos = getTopology( numNodes );
    OmAudioBuffer bufferIn = getaudioBuffer( 2, numSamplesIn );

    
    
    // test OmWoodland methods
    
    OmWoodlandRegisterTopology( &nodesPos );
    // propagationHandler.PrintTopology();
    
    OmWoodlandSetSampleRate ( sampleRate );
    

    OmWoodlandSetSpeedGainRxThreshold( speed, gain, rxThreshold);
    
    unsigned int emitterId = 1;
    OmWoodlandSimulatePropagation( emitterId );
    propagationHandler.PrintTree();
    propagationHandler.PrintIR();
    
    std::cout << "max propag time: " << OmWoodlandGetMaxPropagationTime() << " sec\n";
    
    int numSamplesOut = std::ceil(OmWoodlandGetMaxPropagationTime() * sampleRate) + numSamplesIn;
    OmAudioBuffer bufferOut = getaudioBuffer( 1, numSamplesOut );
    unsigned int nodeId = 1;
    OmWoodlandConvolveInputToOutput( &bufferIn, &bufferOut, nodeId );
    
    return 0;
}
