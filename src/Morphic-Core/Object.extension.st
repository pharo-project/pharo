Extension { #name : 'Object' }

{ #category : '*Morphic-Core' }
Object >> complexContents [

	^ self
]

{ #category : '*Morphic-Core' }
Object >> currentHand [
	"Return a usable HandMorph -- the one associated with the object's current environment.  This method will always return a hand, even if it has to conjure one up as a last resort.  If a particular hand is actually handling events at the moment (such as a remote hand or a ghost hand), it will be returned."

	^ self currentWorld primaryHand
]

{ #category : '*Morphic-Core' }
Object >> currentWorld [
	"Answer a morphic world that is the current UI focus."
	ActiveWorld ifNotNil: [ ^ ActiveWorld ].
	^ World
]

{ #category : '*Morphic-Core' }
Object >> hasModelYellowButtonMenuItems [

	^ Morph cmdGesturesEnabled
]

{ #category : '*Morphic-Core' }
Object >> primitiveError: aString [
	"This method is called when the error handling results in a recursion in
	calling on error: or halt or halt:."

	ErrorHandler default onPrimitiveError: aString
]
