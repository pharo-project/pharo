Extension { #name : #GLMBrickThemer }

{ #category : #'*GT-Playground' }
GLMBrickThemer >> bindingThemer [

	^ self registerModule: GTPlayBindingThemer new
		to: GLMActionBrickThemer new
]
