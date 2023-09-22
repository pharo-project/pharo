Extension { #name : 'Class' }

{ #category : '*RottenTestsFinder' }
Class >> lowestCompiledMethodInInheritanceChainNamed: aSelector [
	"Starting from self, look if myself define a CompiledMethod named aSelector.
	 If not, look in my superclasses following the inheritance chain.
	 If no CompiledMethod is used, raise an error."
	^ (self lookupSelector: aSelector)
		ifNil: [ self error: 'CompiledMethod does not exist: ' , aSelector ]
]
