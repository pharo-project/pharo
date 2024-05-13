"
I am a refactoring for replacing every direct access to  instance  variables with accessor methods.

My precondition verifies that the variable is directly defined in that class.
I create new accessor methods for the variables and replace every read and write to this variable with the new accessors.

"
Class {
	#name : 'RBAbstractInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'accessorsRefactoring'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'transforming' }
RBAbstractInstanceVariableRefactoring >> abstractReferences [

	"Precondition: createAccessors has been executed. Else the setter and getter method are not initialized."

	| replacer |
	replacer := self parseTreeRewriterClass
		            variable: variableName
		            getter: self accessorsRefactoring getterMethodName
		            setter: self accessorsRefactoring setterMethodName.
	self
		convertClasses: class withAllSubclasses
		select: [ :aClass | "remove the setter and getters from the list of methods whose instance variable accesses should be converted to
			getter/setter."
			(aClass whichSelectorsReferToInstanceVariable: variableName)
				reject: [ :each |
					aClass == class and: [
						each == self accessorsRefactoring getterMethodName or: [
							each == self accessorsRefactoring setterMethodName ] ] ] ]
		using: replacer
]

{ #category : 'private - accessing' }
RBAbstractInstanceVariableRefactoring >> accessorsRefactoring [

	^ accessorsRefactoring
		ifNil: [ accessorsRefactoring := RBCreateAccessorsForVariableTransformation
				model: self model
				variable: variableName
				class: class
				classVariable: false
			]
		ifNotNil: [ accessorsRefactoring ]
]

{ #category : 'preconditions' }
RBAbstractInstanceVariableRefactoring >> applicabilityPreconditions [

	^ { (RBCondition
		   directlyDefinesInstanceVariable: variableName
		   in: class) }
]

{ #category : 'transforming' }
RBAbstractInstanceVariableRefactoring >> createAccessors [
	self generateChangesFor: self accessorsRefactoring
]

{ #category : 'preconditions' }
RBAbstractInstanceVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBAbstractInstanceVariableRefactoring >> privateTransform [
	self createAccessors.
	self abstractReferences
]
