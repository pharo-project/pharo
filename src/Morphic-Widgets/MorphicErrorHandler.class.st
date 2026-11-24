"
I am an interactive session manager specific for Morphic
"
Class {
	#name : 'MorphicErrorHandler',
	#superclass : 'InteractiveErrorHandler',
	#category : 'Morphic-Widgets',
	#package : 'Morphic-Widgets'
}

{ #category : 'error handling' }
MorphicErrorHandler >> onPrimitiveError: aString [

	super onPrimitiveError: aString.
	
	"init hands and redisplay"
	self currentWorld install
]
