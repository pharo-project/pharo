Class {
	#name : #RBCompositeVariableTransformation,
	#superclass : #RBCompositeTransformation,
	#instVars : [
		'variableName',
		'isClassVariable',
		'className'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBCompositeVariableTransformation class >> classVariable: aVariableName class: aClassName [

	^ self new
		variable: aVariableName
		class: aClassName
		classVariable: true;
		yourself
]

{ #category : #api }
RBCompositeVariableTransformation class >> instanceVariable: aVariableName class: aClassName [

	^ self new
		variable: aVariableName
		class: aClassName
		classVariable: false;
		yourself
]

{ #category : #api }
RBCompositeVariableTransformation class >> model: aRBModel classVariable: aVariableName class: aClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		class: aClassName
		classVariable: true;
		yourself
]

{ #category : #api }
RBCompositeVariableTransformation class >> model: aRBModel instanceVariable: aVariableName class: aClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		class: aClassName
		classVariable: false;
		yourself
]

{ #category : #api }
RBCompositeVariableTransformation class >> model: aRBModel variable: aVariableName class: aClassName isClassVariable: aBoolean [

	^ self new
		model: aRBModel;
		variable: aVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : #api }
RBCompositeVariableTransformation class >> variable: aVariableName class: aClassName isClassVariable: aBoolean [

	^ self new
		variable: aVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : #accessing }
RBCompositeVariableTransformation >> definingClass [

	^ self model classObjectFor: className
]
