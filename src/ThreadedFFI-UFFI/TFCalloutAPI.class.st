"
I am a TFCalloutAPI implementation that overrides the builder to use the one needed to generate TFFI code
"
Class {
	#name : #TFCalloutAPI,
	#superclass : #FFICalloutAPI,
	#classVars : [
		'TraceBlock'
	],
	#category : #'ThreadedFFI-UFFI'
}

{ #category : #tracing }
TFCalloutAPI class >> activateTrace [

	self activateTraceWith: [ :sender | sender crTrace ]
]

{ #category : #tracing }
TFCalloutAPI class >> activateTraceWith: aBlock [

	TraceBlock := aBlock.
	FFIMethodRegistry resetAll
]

{ #category : #tracing }
TFCalloutAPI class >> isTracing [

	^ TraceBlock notNil
]

{ #category : #'instance creation' }
TFCalloutAPI class >> newCallbackBackendFor: aCallback [

	^ (TFCallback
		forCallback: aCallback
		parameters: (aCallback functionSpec arguments collect: #tfExternalTypeWithArity) asArray
		returnType: aCallback functionSpec returnType tfExternalTypeWithArity
		runner: aCallback ffiLibrary runner forCallback)
			runStrategy: aCallback ffiLibrary callbackRunningStrategy;
			yourself
]

{ #category : #tracing }
TFCalloutAPI class >> resetTrace [
	<script>

	TraceBlock := nil.
	FFIMethodRegistry resetAll
]

{ #category : #tracing }
TFCalloutAPI class >> trace: sender [
	"This method is not to be used directly, it is called by TFFI when #activateTrace was called."

	TraceBlock ifNil: [ ^ self ].
	TraceBlock value: sender
]

{ #category : #action }
TFCalloutAPI >> newBuilder [

	^ TFCalloutMethodBuilder calloutAPI: self
]
