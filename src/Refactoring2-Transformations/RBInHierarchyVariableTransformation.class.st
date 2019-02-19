"
I am an abstract class for transformation on variables that happens in the same hierarchy.
"
Class {
	#name : #RBInHierarchyVariableTransformation,
	#superclass : #RBCompositeVariableTransformation,
	#instVars : [
		'isClassVariable'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBInHierarchyVariableTransformation class >> classVariable: aVariableName class: aClassName [
	^ self new
		variable: aVariableName class: aClassName classVariable: true;
		yourself
]

{ #category : #api }
RBInHierarchyVariableTransformation class >> instanceVariable: aVariableName class: aClassName [
	^ self new
		variable: aVariableName class: aClassName classVariable: false;
		yourself
]

{ #category : #api }
RBInHierarchyVariableTransformation class >> model: aRBModel classVariable: aVariableName class: aClassName [
	^ self new
		model: aRBModel;
		variable: aVariableName class: aClassName classVariable: true;
		yourself
]

{ #category : #api }
RBInHierarchyVariableTransformation class >> model: aRBModel instanceVariable: aVariableName class: aClassName [
	^ self new
		model: aRBModel;
		variable: aVariableName class: aClassName classVariable: false;
		yourself
]

{ #category : #printing }
RBInHierarchyVariableTransformation >> storeOn: aStream [
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
RBInHierarchyVariableTransformation >> variable: aVariableName class: aClassName classVariable: aBoolean [
	variableName := aVariableName.
	isClassVariable := aBoolean.
	className := aClassName
]
