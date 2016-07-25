tests-pattern
testAddClassPattern
	"Make sure that all class definitions can be parsed."

	self exampleClasses do: [ :class |
		(class isObsolete or: [ class superclass notNil and: [ class superclass isObsolete ] ]) ifFalse: [
			| change |
			change := changes defineClass: class definition.
			self assert: (change isKindOf: RBAddClassChange).
			self assert: (change changeClassName = class name).
			self assert: (class superclass isNil
				ifTrue: [ change definitionClass = ProtoObject ]
				ifFalse: [ change definitionClass = class superclass ]).
			self assert: (change instanceVariableNames = class instVarNames asArray).
			self assert: (change classVariableNames = class classVarNames asArray).
			self assert: (change sharedPoolNames = class sharedPoolNames asArray).
			self assert: (change category = class category).
			self universalTestFor: change ] ]