Extension { #name : 'TaAbstractComposition' }

{ #category : '*Refactoring-Core' }
TaAbstractComposition >> defineBehavior: aBlock in: aNamespace [
	"My traits are installed, so the new behavior can use me right away."

	^ aNamespace defineBehavior: [ :builder |
		aBlock value: builder.
		builder traitComposition: self ]
]
