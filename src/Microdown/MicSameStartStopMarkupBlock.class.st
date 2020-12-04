"
I'm abstract block for the logic of elements which are managed by a 

startMarkup
body 
endMarkup 

My subclass should define lineMarkup
"
Class {
	#name : #MicSameStartStopMarkupBlock,
	#superclass : #MicStartStopMarkupBlock,
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicSameStartStopMarkupBlock class >> isAbstract [

	^ self == MicSameStartStopMarkupBlock
]

{ #category : #markups }
MicSameStartStopMarkupBlock >> lineStopMarkup [

	^ self lineStartMarkup
]
