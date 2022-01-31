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
	#instVars : [
		'arguments'
	],
	#category : #'Microdown-Model'
}

{ #category : #testing }
MicSameStartStopMarkupBlock class >> isAbstract [

	^ self == MicSameStartStopMarkupBlock
]

{ #category : #accessing }
MicSameStartStopMarkupBlock >> argumentAssociations [
	^ arguments associations
]

{ #category : #accessing }
MicSameStartStopMarkupBlock >> arguments: aDictionary [
	arguments := aDictionary
]

{ #category : #visiting }
MicSameStartStopMarkupBlock >> caption [
	^ String streamContents: [:s |  self captionElements do: [ :each | s nextPutAll: each substring ] ]
]

{ #category : #visiting }
MicSameStartStopMarkupBlock >> captionElements [
	^ arguments at: #caption ifAbsent: [ #() ]
]

{ #category : #visiting }
MicSameStartStopMarkupBlock >> closeMe [ 
	
	super closeMe.
	self arguments at: #caption 
		ifPresent: [:cp| arguments at: #caption put: (self inlineParse: cp) ]
		
]

{ #category : #accessing }
MicSameStartStopMarkupBlock >> hasCaption [
	^ arguments includesKey:  #caption
]

{ #category : #markups }
MicSameStartStopMarkupBlock >> lineStopMarkup [

	^ self lineStartMarkup
]
