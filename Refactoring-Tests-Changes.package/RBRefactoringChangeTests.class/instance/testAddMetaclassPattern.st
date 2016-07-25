tests-pattern
testAddMetaclassPattern
	"Make sure that metaclass definitions can be parsed."
	
	self exampleClasses do: [ :class |
		(class isObsolete or: [ class superclass notNil and: [ class superclass isObsolete ] ]) ifFalse: [  
 		| change |
		change := changes defineClass: class class definition.
		self assert: (change isKindOf: RBAddMetaclassChange).
		self assert: change changeClassName equals: class name.
		self assert: change classInstanceVariableNames equals: class class instVarNames.
		self universalTestFor: change ]]