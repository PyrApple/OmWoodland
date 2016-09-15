/************************************************************************************/
/*!
 *  @file       OmWoodland
 *  @brief      Propagation in a forest-like environment
 *  @author     David Poirier-Quinot
 *  @date       09/2016
 *
 */
/************************************************************************************/
#ifndef _OM_WOODLAND_H__
#define _OM_WOODLAND_H__

/// @n for now this is only for mac (gcc or clang)
#define OM_WOODLAND_VISIBILITY_DEFAULT	__attribute__ ((visibility ("default")))

// use this export macro to expose public method to the dylib
#ifdef __cplusplus
#define OM_WOODLAND_C_EXPORTS   extern "C"
#else
#define OM_WOODLAND_C_EXPORTS
#endif

#define OM_WOODLAND_API OM_WOODLAND_C_EXPORTS OM_WOODLAND_VISIBILITY_DEFAULT

#include <stdbool.h>    ///< include boolean for C interface
#include <iostream>

/************************************************************************************/

struct OM_WOODLAND_VISIBILITY_DEFAULT OmList
{
    unsigned int size;
    float * data;
};
typedef struct OmList OmList;     ///< C-style declaration

struct OM_WOODLAND_VISIBILITY_DEFAULT OmListOfList
{
    unsigned int size1;
    unsigned int size2;
    float ** data;
};
typedef struct OmListOfList OmListOfList;     ///< C-style declaration

struct OM_WOODLAND_VISIBILITY_DEFAULT OmAudioBuffer
{
    unsigned int numChannels;  // < number of channels
    unsigned int numSamples;  // < number of samples (for each channel)
    float ** data;             // < data[ channelIndex ][ sampleIndex ]
};
typedef struct OmAudioBuffer OmAudioBuffer;     ///< C-style declaration

/************************************************************************************/

#include "propagationHandler.hpp"
#include "utils.hpp"

static PropagationHandler propagationHandler;

OM_WOODLAND_API
void OmWoodlandRegisterTopology( OmListOfList * nodesPos )
{
    propagationHandler.RegisterTopology( nodesPos );
    propagationHandler.PrintTopology();
}

OM_WOODLAND_API
void OmWoodlandSimulatePropagation( unsigned int emitterId )
{

    propagationHandler.CreateTree( emitterId );
    propagationHandler.GrowTree( );
    
    // propagationHandler.PrintIR();
}

OM_WOODLAND_API
float OmWoodlandGetMaxPropagationTime()
{
    return propagationHandler.GetMaxPropagationTime();
}

OM_WOODLAND_API
void OmWoodlandSetSampleRate( unsigned int sampleRate )
{
    propagationHandler.sampleRate = sampleRate;
}

OM_WOODLAND_API
void OmWoodlandSetSpeedGainRxThreshold( float speed, float gain, float rxThreshold)
{
    propagationHandler.PROPAGATION_SPEED = speed;
    propagationHandler.PROPAGATION_GAIN = gain;
    propagationHandler.RX_GAIN_THRESHOLD = rxThreshold;
}

OM_WOODLAND_API
void OmWoodlandConvolveInputToOutput( OmAudioBuffer * const bufferIn, OmAudioBuffer * const bufferOut )
{
    propagationHandler.ConvolveIR(bufferIn, bufferOut);
}






#endif /* _OM_CWRAP_H__ */



























