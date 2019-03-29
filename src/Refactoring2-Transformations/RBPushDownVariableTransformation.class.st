"
Removes this variable from the given class, then adds this variable in all the subclasses of this class.

Usage:
transformation := (RBPushDownVariableTransformation 
		instanceVariable: 'foo1'
		class: #RBLintRuleTest)
		transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
overriden from RBAddVariableTransformation and RBRemoveVariableAccessorTransformation
"
Class {
	#name : #RBPushDownVariableTransformation,
	#superclass : #RBCompositeVariableTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #converting }
RBPushDownVariableTransformation >> asRefactoring [

	^ super asRefactoring
		delegatesPreconditions: false;
		yourself
]

{ #category : #executing }
RBPushDownVariableTransformation >> buildTransformations [

	^ OrderedCollection new
		add: (RBRemoveVariableTransformation
				model: self model
				variable: variableName
				class: className
				classVariable: isClassVariable);
		addAll: (self variableDefinitionsInHierarchy
			collect: [ :each | 
				RBAddVariableTransformation
				model: self model
				variable: variableName
				class: each
				classVariable: isClassVariable ]);
		yourself
]

{ #category : #preconditions }
RBPushDownVariableTransformation >> preconditions [
	"it does not perform when two subclasses
	(and/or their subclasses) access this variable"

	^ isClassVariable
		ifTrue: [ self preconditionsForClassVariable ]
		ifFalse: [ self preconditionsForInstanceVariable ]
]

{ #category : #preconditions }
RBPushDownVariableTransformation >> preconditionsForClassVariable [
	"it does not perform when two subclasses (and/or their subclasses) accesses this variable"

	^ (RBCondition definesClassVariable: variableName in: self definingClass)
		& (RBCondition withBlock: [
			| numSubclassesAccessing |
			numSubclassesAccessing := self definingClass subclasses count: [ :subclass |
				subclass withAllSubclasses anySatisfy: [ :e |
					(e whichSelectorsReferToClassVariable: variableName) isNotEmpty
					or: [ (e theMetaClass whichSelectorsReferToClassVariable: variableName) isNotEmpty ] ] ].
			numSubclassesAccessing < 2 ])
]

{ #category : #preconditions }
RBPushDownVariableTransformation >> preconditionsForInstanceVariable [

	^ (RBCondition definesInstanceVariable: variableName in: self definingClass) 
		& (RBCondition accessesInstanceVariable: variableName in: self definingClass showIn: self)
]

{ #category : #printing }
RBPushDownVariableTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	className storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	isClassVariable storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #api }
RBPushDownVariableTransformation >> variable: aVariableName class: aClassName classVariable: aBoolean [

	variableName := aVariableName.
	isClassVariable := aBoolean.
	className := aClassName.
]

{ #category : #private }
RBPushDownVariableTransformation >> variableDefinitionsInHierarchy [

	self flag: #pharoTodo. "why the behavior is different for class and instance variable?"
	^ isClassVariable
		ifTrue: [ self definingClass allSubclasses reject: [ :each |
			(each whichSelectorsReferToClassVariable: variableName) isEmpty
			and: [ (each theMetaClass whichSelectorsReferToClassVariable: variableName) isEmpty ] ] ]
		ifFalse: [ self definingClass subclasses ] 
]
