class addition
addClassDefinitionName: aClassName
	"aClassName should only be a class (and not a metaclass name)"

	| aClassNameSymbol |
	('* class' match: aClassName)
		ifTrue: [ ^ self error: 'no metaclass name' ].
	aClassNameSymbol := aClassName asSymbol.
	(classes includes: aClassNameSymbol)
		ifTrue: [ ^ self ].
	classes add: aClassNameSymbol.
	self extendedMethodsShouldBecomeDefinedWhenAddingClassName: aClassNameSymbol.
	self registerClassName: aClassNameSymbol