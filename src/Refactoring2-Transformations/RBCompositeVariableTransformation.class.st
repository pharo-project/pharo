Class {
	#name : #RBCompositeVariableTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		'variableName',
		'className'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #accessing }
RBCompositeVariableTransformation >> definingClass [

	^ self model classObjectFor: className
]
