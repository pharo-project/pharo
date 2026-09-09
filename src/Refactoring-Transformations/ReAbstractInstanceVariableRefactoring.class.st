"
I am a refactoring for replacing every direct access to  instance  variables with accessor methods.

My precondition verifies that the variable is directly defined in that class.
I create new accessor methods for the variables and replace every read and write to this variable with the new accessors.

"
Class {
	#name : 'ReAbstractInstanceVariableRefactoring',
	#superclass : 'RBVariableRefactoring',
	#instVars : [
		'accessorsRefactoring'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'transforming' }
ReAbstractInstanceVariableRefactoring >> abstractReferences [

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
ReAbstractInstanceVariableRefactoring >> accessorsRefactoring [

	^ accessorsRefactoring
		ifNil: [ accessorsRefactoring := ReCreateAccessorsForVariableTransformation
				model: self model
				variable: variableName
				class: class
				classVariable: false
			]

]

{ #category : 'preconditions' }
ReAbstractInstanceVariableRefactoring >> applicabilityPreconditions [

	^ { (RBCondition
		   directlyDefinesInstanceVariable: variableName
		   in: class) }
]

{ #category : 'transforming' }
ReAbstractInstanceVariableRefactoring >> createAccessors [
	self generateChangesFor: self accessorsRefactoring
]

{ #category : 'preconditions' }
ReAbstractInstanceVariableRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
ReAbstractInstanceVariableRefactoring >> privateTransform [
	self createAccessors.
	self abstractReferences
]
