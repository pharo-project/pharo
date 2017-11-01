Class {
	#name : #GLMMorphicPopupActionRenderer,
	#superclass : #GLMMorphicActionRenderer,
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #accessing }
GLMMorphicPopupActionRenderer >> actionArguments [
	^ nil
]

{ #category : #accessing }
GLMMorphicPopupActionRenderer >> actionSelector [
	^ #popupActOn:from:
]
