"
#printString is implemented by manipulating a stream.
Using it inside a #printOn: will imply a useless creation of stream and its pass to #printOn: one, instead of using directely the stream parameter to #printOn:
"
Class {
	#name : 'ReNoPrintStringInPrintOnRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Tests-Optimization',
	#package : 'General-Rules-Tests',
	#tag : 'Optimization'
}

{ #category : 'testing - interest' }
ReNoPrintStringInPrintOnRule class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReNoPrintStringInPrintOnRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReNoPrintStringInPrintOnRule class >> rationale [
	^ 'Use the stream argument to #printOn: e.g.( stream print: anObject ) instead of creating an extra stream by using printString. e.g.( stream nextPutAll: anObject printString ).'
]

{ #category : 'accessing' }
ReNoPrintStringInPrintOnRule class >> ruleName [
	^ 'No printString inside printOn'
]

{ #category : 'running' }
ReNoPrintStringInPrintOnRule >> check: aMethod forCritiquesDo: aCriticBlock [
	| problemPrintStrings |
	aMethod selector = #printOn: ifFalse: [ ^self ].
	problemPrintStrings :=
		aMethod sendNodes select: [ :msgSend | msgSend selector = #printString].

	problemPrintStrings do: [ :msgSend |
			aCriticBlock cull: (self
				createTrivialCritiqueOn: aMethod
				intervalOf: msgSend
				hint: msgSend selector asString )]
]
