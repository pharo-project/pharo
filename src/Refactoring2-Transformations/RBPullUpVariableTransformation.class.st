"
Removes this variable from all the subclasses of a class, then adds this variable in this class. If none of the subclasses define a variable with the given name, this transformation works as a RBAddVariableTransformation.

Usage:
transformation := (RBPullUpVariableTransformation
		instanceVariable: 'result'
		class: #RBLintRuleTest)
		transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
overriden from RBRemoveVariableTransformation and RBAddVariableAccessorTransformation
"
Class {
	#name : #RBPullUpVariableTransformation,
	#superclass : #RBInHierarchyVariableTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #executing }
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

{ #category : #private }
RBPullUpVariableTransformation >> variableDefinitionsInHierarchy [

	^ self definingClass allSubclasses
		select: [ :each |
			isClassVariable
				ifTrue: [ each isMeta not and: [ each directlyDefinesClassVariable: variableName ] ]
				ifFalse: [ each directlyDefinesInstanceVariable: variableName ] ]
		thenCollect: #name
	
	
]
