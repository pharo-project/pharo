"
Used to test if all defined arguments of a condition or not nil.

We use this as a default condition for presentations. Usually, you will want to define a condition as a block such as '[ :class :category | (class isKindOf: Class) and: [ category isKindOf: Symbol ] ]' (or something like that) but sometimes you just want to make sure that all arguments are defined. Since we do not know the number of arguments in advance, we use this class to interpret #glamourValueWithArgs: and return true if all arguments are not nil.
"
Class {
	#name : #GLMAllNotNilCondition,
	#superclass : #GLMCondition,
	#category : #Glamour-Helpers
}

{ #category : #glamour }
GLMAllNotNilCondition >> glamourValueWithArgs: anArray [ 
	 
	^anArray allSatisfy: [:each | each isNil not]
]
