Class {
	#name : 'ReGenerateAbstractTransformation',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'variables'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'preconditions' }
ReGenerateAbstractTransformation >> applicabilityPreconditions [
	"Checks that the class actually defines all the variables that will be part of the printOn:."

	^ variables collect: [ :variable |
		  RBCondition definesInstanceVariable: variable in: self theClass ]
]

{ #category : 'accessing' }
ReGenerateAbstractTransformation >> theClass [
	^ (self classObjectFor: className) instanceSide
]

{ #category : 'accessing' }
ReGenerateAbstractTransformation >> variables [
	"Answer a <Collection> of <Symbol> each one representing a variable in the receiver's target class"
	
	^ variables 
]

{ #category : 'accessing' }
ReGenerateAbstractTransformation >> variables: anArray [
	variables := anArray
]
