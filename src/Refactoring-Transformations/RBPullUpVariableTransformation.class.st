"
Removes this variable from all the subclasses of a class, then adds this variable in this class. If none of the subclasses define a variable with the given name, this transformation works as a RBAddVariableTransformation.

Usage:
```
| transformation |
transformation := (RBPullUpVariableTransformation
		instanceVariable: 'result'
		class: #RBBasicLintRuleTestData)
		generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
overriden from RBRemoveVariableTransformation and RBAddVariableAccessorTransformation
"
Class {
	#name : 'RBPullUpVariableTransformation',
	#superclass : 'RBCompositeVariableTransformation',
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'preconditions' }
RBPullUpVariableTransformation >> applicabilityPreconditions [

	^ { (isClassVariable
		   ifTrue: [
			   (self definingClass hierarchyDefinesClassVariable: variableName)
				   ifFalse: [
				   self refactoringError: 'No subclass defines ' , variableName ].
			   (RBCondition isMetaclass: self definingClass) not ]
		   ifFalse: [
			   RBCondition withBlock: [
				   (self definingClass hierarchyDefinesInstanceVariable:
					    variableName) ifFalse: [
					   self refactoringError: 'No subclass defines ' , variableName ].
				   true ] ]) }
]

{ #category : 'executing' }
RBPullUpVariableTransformation >> buildTransformations [

	^ OrderedCollection new
		addAll: (self variableDefinitionsInHierarchy
			collect: [ :each |
				RBRemoveVariableTransformation
				model: self model
				variable: variableName
				class: each
				classVariable: isClassVariable ]);
		add: (RBAddVariableTransformation
				model: self model
				variable: variableName
				class: className
				classVariable: isClassVariable);
		yourself
]

{ #category : 'storing' }
RBPullUpVariableTransformation >> storeOn: aStream [

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

{ #category : 'api' }
RBPullUpVariableTransformation >> variable: aVariableName class: aClassName classVariable: aBoolean [

	variableName := aVariableName.
	isClassVariable := aBoolean.
	className := aClassName
]

{ #category : 'private' }
RBPullUpVariableTransformation >> variableDefinitionsInHierarchy [

	^ self definingClass allSubclasses
		select: [ :each |
			isClassVariable
				ifTrue: [ each isMeta not and: [ each directlyDefinesClassVariable: variableName ] ]
				ifFalse: [ each directlyDefinesInstanceVariable: variableName ] ]
		thenCollect: #name
]
