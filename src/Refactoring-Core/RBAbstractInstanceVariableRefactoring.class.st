"
I am a refactoring for replacing every direct access to  instance  variables with accessor methods.

My precondition verifies that the variable is directly defined in that class.
I create new accessor methods for the variables and replace every read and write to this variable with the new accessors.

"
Class {
	#name : #RBAbstractInstanceVariableRefactoring,
	#superclass : #RBVariableRefactoring,
	#instVars : [
		'accessorsRefactoring'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #transforming }
RBAbstractInstanceVariableRefactoring >> abstractReferences [
	| replacer |
	replacer := RBParseTreeRewriter 
				variable: variableName
				getter: self accessorsRefactoring getterMethod
				setter: self accessorsRefactoring setterMethod.
	self 
		convertClasses: class withAllSubclasses
		select: 
			[:aClass | 
			(aClass whichSelectorsReferToInstanceVariable: variableName) reject: 
					[:each | 
					aClass == class and: 
							[each == self accessorsRefactoring getterMethod 
								or: [each == self accessorsRefactoring setterMethod]]]]
		using: replacer
]

{ #category : #'private-accessing' }
RBAbstractInstanceVariableRefactoring >> accessorsRefactoring [

	^ accessorsRefactoring
		ifNil: [ accessorsRefactoring := RBCreateAccessorsForVariableRefactoring
				model: self model
				variable: variableName
				class: class
				classVariable: false
			]
		ifNotNil: [ accessorsRefactoring ]
]

{ #category : #transforming }
RBAbstractInstanceVariableRefactoring >> createAccessors [
	self performComponentRefactoring: self accessorsRefactoring
]

{ #category : #preconditions }
RBAbstractInstanceVariableRefactoring >> preconditions [
	^RBCondition directlyDefinesInstanceVariable: variableName in: class
]

{ #category : #transforming }
RBAbstractInstanceVariableRefactoring >> transform [
	self createAccessors.
	self abstractReferences
]
