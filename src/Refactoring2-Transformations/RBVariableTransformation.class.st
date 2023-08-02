Class {
	#name : #RBVariableTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'variableName',
		'isClassVariable'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #api }
RBVariableTransformation class >> classVariable: aVariableName class: aClassName [

	^ self new
		variable: aVariableName
		class: aClassName
		classVariable: true;
		yourself
]

{ #category : #api }
RBVariableTransformation class >> instanceVariable: aVariableName class: aClassName [

	^ self new
		variable: aVariableName
		class: aClassName
		classVariable: false;
		yourself
]

{ #category : #api }
RBVariableTransformation class >> model: aRBModel classVariable: aVariableName class: aClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		class: aClassName
		classVariable: true;
		yourself
]

{ #category : #api }
RBVariableTransformation class >> model: aRBModel instanceVariable: aVariableName class: aClassName [

	^ self new
		model: aRBModel;
		variable: aVariableName
		class: aClassName
		classVariable: false;
		yourself
]

{ #category : #api }
RBVariableTransformation class >> model: aRBModel variable: aVariableName class: aClassName classVariable: aBoolean [

	^ self new
		model: aRBModel;
		variable: aVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : #api }
RBVariableTransformation class >> variable: aVariableName class: aClassName classVariable: aBoolean [

	^ self new
		variable: aVariableName
		class: aClassName
		classVariable: aBoolean;
		yourself
]

{ #category : #accessing }
RBVariableTransformation >> definingClass [
	"Usually a shared variable is defined on the instance side and instance 
	variables on both instance and class side."
	
	^ isClassVariable
		ifTrue: [ super definingClass instanceSide ]
		ifFalse: [ super definingClass ]
]

{ #category : #printing }
RBVariableTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	isClassVariable storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #api }
RBVariableTransformation >> variable: aVariableName class: aClassName classVariable: aBoolean [

	self className: aClassName.
	variableName := aVariableName.
	isClassVariable := aBoolean
]
