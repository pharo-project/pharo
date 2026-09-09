Extension { #name : 'Object' }

{ #category : '*Morphic-Core' }
Object >> complexContents [

	^ self
]

{ #category : '*Morphic-Core' }
Object >> currentWorld [
	"Answer a morphic world that is the current UI focus."
	ActiveWorld ifNotNil: [ ^ ActiveWorld ].
	^ World
]

{ #category : '*Morphic-Core' }
Object >> primitiveError: aString [
	"This method is called when the error handling results in a recursion in
	calling on error: or halt or halt:."

	ErrorHandler default onPrimitiveError: aString
]
