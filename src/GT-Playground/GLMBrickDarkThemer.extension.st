Extension { #name : #GLMBrickDarkThemer }

{ #category : #'*GT-Playground' }
GLMBrickDarkThemer >> bindingThemer [

	^ self registerModule: GTPlayBindingDarkThemer new
		to: GLMActionBrickDarkThemer new
]
