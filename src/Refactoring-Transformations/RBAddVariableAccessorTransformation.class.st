"
Adds accessors (getter and setter) for a variable in a class, if they do not exist.

Usage:
```
| transformation |
transformation := (RBAddVariableAccessorTransformation
	variable: 'variableName'
	class: #RBVariableTransformation
	classVariable: false)
		generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the variable with which the accessors will be created shall exist. The parameter isClassVariable indicates whether to look in the instance or class variables.
"
Class {
	#name : 'RBAddVariableAccessorTransformation',
	#superclass : 'RBVariableTransformation',
	#instVars : [
		'getterMethod',
		'setterMethod'
	],
	#category : 'Refactoring-Transformations-Model',
	#package : 'Refactoring-Transformations',
	#tag : 'Model'
}

{ #category : 'preconditions' }
RBAddVariableAccessorTransformation >> applicabilityPreconditions [

	class := self model classObjectFor: className.

	^ { (isClassVariable
		   ifTrue: [
				RBCondition definesClassVariable: variableName asSymbol in: class ]
		   ifFalse: [
				RBCondition definesInstanceVariable: variableName in: class ]) }
]

{ #category : 'private' }
RBAddVariableAccessorTransformation >> createGetterAccessor [

	(self definingClass getterMethodFor: variableName) ifNil: [ self defineGetterMethod ]
]

{ #category : 'private' }
RBAddVariableAccessorTransformation >> createSetterAccessor [

	(self definingClass setterMethodFor: variableName)
		ifNil: [ self defineSetterMethod ]
		ifNotNil: [ setterMethod := self definingClass setterMethodFor: variableName ]
]

{ #category : 'private' }
RBAddVariableAccessorTransformation >> defineGetterMethod [

	getterMethod := self safeMethodNameFor: self definingClass basedOn: variableName asString.
	self generateChangesFor: (RBAddMethodTransformation
		model: self model 
		sourceCode: ('<1s><r><r><t>^ <2s>' expandMacrosWith: getterMethod with: variableName)
		in: self definingClass
		withProtocol: 'accessing').

	^ getterMethod
]

{ #category : 'private' }
RBAddVariableAccessorTransformation >> defineSetterMethod [

	|  sourceCode |
	sourceCode := '<1s> anObject<r><r><t><2s> := anObject'.
	setterMethod := self safeMethodNameFor: self definingClass basedOn: variableName asString , ':'.
	self generateChangesFor: (RBAddMethodTransformation 
		model: self model
		sourceCode: (sourceCode expandMacrosWith: setterMethod with: variableName)
		in: self definingClass 
		withProtocol: 'accessing').
	^ setterMethod
]

{ #category : 'accessing' }
RBAddVariableAccessorTransformation >> definingClass [
	"Usually a shared variable is defined on the instance side and instance 
	variables on both instance and class side."
	
	^ isClassVariable
		ifTrue: [ super definingClass classSide ]
		ifFalse: [ super definingClass ]
]

{ #category : 'private' }
RBAddVariableAccessorTransformation >> getterMethod [
	^ getterMethod
]

{ #category : 'executing' }
RBAddVariableAccessorTransformation >> privateTransform [

	self
		createGetterAccessor;
		createSetterAccessor
]

{ #category : 'private' }
RBAddVariableAccessorTransformation >> setterMethod [
	^ setterMethod
]
