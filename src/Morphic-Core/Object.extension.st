Extension { #name : #Object }

{ #category : #'*Morphic-Core' }
Object >> complexContents [

	^self
]

{ #category : #'*Morphic-Core' }
Object >> currentWorld [
	"Answer a morphic world that is the current UI focus."
	^ UIManager default currentWorld
]
