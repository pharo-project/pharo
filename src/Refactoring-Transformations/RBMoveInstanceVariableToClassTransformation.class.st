"
Moves an instance variable from a class to another. It does not check whether this variable is being referenced in the former class.

Usage:
| transformation |
transformation := (RBMoveInstanceVariableToClassTransformation
				variable: 'methodBlock'
				fromClass: #RBBasicLintRuleTest
				toClass: #RBFooLintRuleTest)
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
overrides the preconditions from both RBAddVariableRefactoring and RBRemoveVariableRefactoring
"
Class {
	#name : 'RBMoveInstanceVariableToClassTransformation',
	#superclass : 'RBCompositeVariableTransformation',
	#instVars : [
		'oldClassName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBMoveInstanceVariableToClassTransformation class >> model: aRBModel variable: aVariableName fromClass: fromClassName toClass: toClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		fromClass: fromClassName
		toClass: toClassName;
		yourself
]

{ #category : 'api' }
RBMoveInstanceVariableToClassTransformation class >> variable: aVariableName fromClass: fromClassName toClass: toClassName [

	^ self new
		variable: aVariableName
		fromClass: fromClassName
		toClass: toClassName;
		yourself
]

{ #category : 'executing' }
RBMoveInstanceVariableToClassTransformation >> buildTransformations [

	^ { (RBAddVariableTransformation
				model: self model
				instanceVariable: variableName asString
				class: className) .
		 (RBRemoveVariableTransformation
				model: self model
				instanceVariable: variableName asString
				class: oldClassName) }
]

{ #category : 'printing' }
RBMoveInstanceVariableToClassTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' fromClass: '.
	oldClassName storeOn: aStream.
	aStream nextPutAll: ' toClass: '.
	className storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'api' }
RBMoveInstanceVariableToClassTransformation >> variable: aVariableName fromClass: fromClassName toClass: toClassName [
	
	variableName := aVariableName.
	className := toClassName.
	oldClassName := fromClassName
]
