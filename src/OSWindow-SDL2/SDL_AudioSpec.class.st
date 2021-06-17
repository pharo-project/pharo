"
I provide a description for opening an audio device.
"
Class {
	#name : #'SDL_AudioSpec',
	#superclass : #SDL2Structure,
	#classVars : [
		'OFFSET_CALLBACK',
		'OFFSET_CHANNELS',
		'OFFSET_FORMAT',
		'OFFSET_FREQ',
		'OFFSET_PADDING',
		'OFFSET_SAMPLES',
		'OFFSET_SILENCE',
		'OFFSET_SIZE',
		'OFFSET_USERDATA'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #'field definition' }
SDL_AudioSpec class >> fieldsDesc [
	"
	self rebuildFieldAccessors
	"
	^#(
	 int freq;
	
	 SDL_AudioFormat format;
	 Uint8 channels;
	 Uint8 silence;

	 Uint16 samples;
	 Uint16 padding;

	 Uint32 size;
	 SDL_AudioCallback callback;
	 void* userdata;
 	)
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> callback [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_CALLBACK) type: ExternalType void asPointerType
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> callback: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_CALLBACK put: anObject getHandle.
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> channels [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_CHANNELS
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> channels: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_CHANNELS put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> format [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_FORMAT
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> format: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_FORMAT put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> freq [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_FREQ
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> freq: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_FREQ put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> padding [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_PADDING
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> padding: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_PADDING put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> samples [
	"This method was automatically generated"
	^handle unsignedShortAt: OFFSET_SAMPLES
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> samples: anObject [
	"This method was automatically generated"
	handle unsignedShortAt: OFFSET_SAMPLES put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> silence [
	"This method was automatically generated"
	^handle unsignedByteAt: OFFSET_SILENCE
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> silence: anObject [
	"This method was automatically generated"
	handle unsignedByteAt: OFFSET_SILENCE put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> size [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_SIZE
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> size: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_SIZE put: anObject
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> userdata [
	"This method was automatically generated"
	^ExternalData fromHandle: (handle pointerAt: OFFSET_USERDATA) type: ExternalType void asPointerType
]

{ #category : #'accessing - structure variables' }
SDL_AudioSpec >> userdata: anObject [
	"This method was automatically generated"
	handle pointerAt: OFFSET_USERDATA put: anObject getHandle.
]
