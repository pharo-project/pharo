"
Creates another class with a subset of instance variables from an existing class. References to instance variables in the original class will be replaced by invocations to getters and setters on the new class.

Usage:
transformation := (RBSplitClassTransformation
	class: #RBRemoveDirectAccessToVariableTransformation
	instanceVariables: #(receiver)
	newClassName: #RBRemoveAccessWithReceiverTransformation
	referenceVariableName: #newReceiver)
	transform.
(ChangesBrowser changes: transformation model changes changes) open

In this example,
the class ""RBRemoveAccessWithReceiverTransformation"" will be created with the variable ""receiver"". The variable ""receiver"" will be removed from the original class ""RBRemoveDirectAccessToVariableTransformation"". All references to ""receiver"" in this class and its subclasses will be replaced by: ""newReceiver receiver ..."" or ""newReceiver receiver: ...""

Preconditions::
-  the instance variable exists
"
Class {
	#name : #RBSplitClassTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		'class',
		'instanceVariables',
		'newClassName',
		'referenceVariableName'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBSplitClassTransformation class >> class: aClass instanceVariables: instVars newClassName: className referenceVariableName: newVariable [
 
	^ self new
		class: aClass
		instanceVariables: instVars
		newClassName: className
		referenceVariableName: newVariable
]

{ #category : #api }
RBSplitClassTransformation class >> model: aRBModel class: aClass instanceVariables: instVars newClassName: className referenceVariableName: newVariable [
 
	^ self new
		model: aRBModel;
		class: aClass
		instanceVariables: instVars
		newClassName: className
		referenceVariableName: newVariable
]

{ #category : #private }
RBSplitClassTransformation >> abstractReferenceTo: instVar [

	transformations add:
		(RBAddVariableAccessorTransformation
			model: self model 
			instanceVariable: instVar
			class: newClassName).
	
	transformations add: ((
		RBRemoveDirectAccessToVariableTransformation
			model: self model 
			instanceVariable: instVar
			class: class)
			receiver: referenceVariableName).
		
	transformations add: (
		RBRemoveVariableTransformation
			model: self model 
			instanceVariable: instVar
			class: class)
]

{ #category : #private }
RBSplitClassTransformation >> abstractVariableReferences [

	instanceVariables do: [ :instVar | self abstractReferenceTo: instVar ]
]

{ #category : #private }
RBSplitClassTransformation >> addClass [

	transformations add: 
		(RBAddClassTransformation 
			model: self model 
			addClass: newClassName 
			superclass: #Object
			subclasses: #()
			category: class category)
]

{ #category : #private }
RBSplitClassTransformation >> addInstanceVariables [

	instanceVariables
		collect: [ :instVar |
			RBAddVariableTransformation
				model: self model 
				instanceVariable: instVar
				class: newClassName ]
		thenDo: [ :each | transformations add: each ]
]

{ #category : #executing }
RBSplitClassTransformation >> buildTransformations [

	transformations := OrderedCollection new.
	
	self
		createNewClass;
		createReference;
		abstractVariableReferences.
		
	^ transformations
		
]

{ #category : #api }
RBSplitClassTransformation >> class: aClass instanceVariables: instVars newClassName: className referenceVariableName: newVariable [
 
	class := self model classObjectFor: aClass.
	instanceVariables := instVars.
	newClassName := className.
	referenceVariableName := newVariable
]

{ #category : #private }
RBSplitClassTransformation >> createNewClass [

	self
		addClass;
		addInstanceVariables
]

{ #category : #private }
RBSplitClassTransformation >> createReference [

	transformations add: (
		RBAddVariableTransformation 
			model: self model 
			instanceVariable: referenceVariableName
			class: class)
]

{ #category : #private }
RBSplitClassTransformation >> definingClass [

	^ self model classObjectFor: class 
]

{ #category : #preconditions }
RBSplitClassTransformation >> preconditions [

	class := self definingClass.

	"check whether instVar exist"
	^(RBCondition isValidClassName: newClassName) 
		& (RBCondition isGlobal: newClassName in: self model) not 
		& (RBCondition isValidInstanceVariableName: referenceVariableName for: class) 
		& (RBCondition hierarchyOf: class definesVariable: referenceVariableName) 
				not 
		& (RBCondition isGlobal: referenceVariableName in: self model) not 
		& (RBCondition definesTemporaryVariable: referenceVariableName in: class) 
				not
]

{ #category : #printing }
RBSplitClassTransformation >> storeOn: aStream [ 

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' instanceVariables: '.
	instanceVariables asArray storeOn: aStream.
	aStream
		nextPutAll: ' newClassName: #';
		nextPutAll: newClassName;
		nextPutAll: ' referenceVariableName: ''';
		nextPutAll: referenceVariableName;
		nextPutAll: ''')'
]
