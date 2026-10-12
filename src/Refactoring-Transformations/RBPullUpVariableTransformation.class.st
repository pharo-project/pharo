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
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'preconditions' }
RBPullUpVariableTransformation >> applicabilityPreconditions [

	^ isClassVariable
		  ifTrue: [
			  {
				  (RBCondition isMetaclass: self definingClass) not.
				  (ReHierarchyDefinesSharedVariableCondition new
					   class: self definingClass;
					   sharedVariables: { variableName };
					   yourself) } ]
		  ifFalse: [
			  { (ReHierarchyDefinesInstanceVariableCondition new
				    class: self definingClass;
				    instanceVariables: { variableName };
				    yourself) } ]
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
]
