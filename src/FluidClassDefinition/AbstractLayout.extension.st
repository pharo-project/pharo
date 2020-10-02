Extension { #name : #AbstractLayout }

{ #category : #'*FluidClassDefinition' }
AbstractLayout class >> kind [
	"Return an identifier describing uniquely the layout"
	^ self subclassResponsibility
]
