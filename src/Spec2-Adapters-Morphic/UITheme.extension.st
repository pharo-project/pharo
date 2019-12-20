Extension { #name : #UITheme }

{ #category : #'*Spec2-Adapters-Morphic' }
UITheme >> modalBackdropColor [
	self flag: #pharoTodo. "This method should probably be moved to Pharo later."

	^ Color black alpha: 0.2
]
