query
allUnreferencedClassVariablesOf: aClass
	"Answer a list of the names of all the receiver's unreferenced class  
	vars, including those defined in superclasses"
	| aList |
	aList := OrderedCollection new.
	aClass withAllSuperclasses
		reverseDo: [:aSuperClass | aSuperClass classVarNames
				do: [:var | (self allReferencesTo: (aSuperClass classPool associationAt: var)) isEmpty
						ifTrue: [aList add: var]]].
	^ aList