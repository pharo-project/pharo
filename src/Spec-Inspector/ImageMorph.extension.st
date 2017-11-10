Extension { #name : #ImageMorph }

{ #category : #'*Spec-Inspector' }
ImageMorph >> resize: newSize [

	self form: (image scaledToSize: newSize)
]
