Extension { #name : #Color }

{ #category : #'*Athens-Cairo' }
Color >> loadOnCairoCanvas: aCairoCanvas [
	aCairoCanvas
		setSourceR: self red g: self green b: self blue a: self alpha
]
