"
I am signaled when a load (or record, or fetch) is asked and its resolution has errors: nothing was recorded or loaded. I carry the MetacelloResolution, to inspect its findings.
"
Class {
	#name : 'MetacelloResolutionError',
	#superclass : 'MetacelloError',
	#instVars : [
		'resolution'
	],
	#category : 'Metacello-Core-Exceptions',
	#package : 'Metacello-Core',
	#tag : 'Exceptions'
}

{ #category : 'signalling' }
MetacelloResolutionError class >> signalFor: aResolution [

	^ self new
		  resolution: aResolution;
		  signal
]

{ #category : 'accessing' }
MetacelloResolutionError >> messageText [

	^ String streamContents: [ :stream |
			  stream nextPutAll: 'The resolution has errors, nothing was loaded:'.
			  resolution errors do: [ :each |
					  stream
						  lf;
						  nextPutAll: '- ';
						  nextPutAll: each text ] ]
]

{ #category : 'accessing' }
MetacelloResolutionError >> resolution [

	^ resolution
]

{ #category : 'accessing' }
MetacelloResolutionError >> resolution: aResolution [

	resolution := aResolution
]
