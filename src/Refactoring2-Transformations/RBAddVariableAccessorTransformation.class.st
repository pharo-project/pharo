"
Adds accessors (getter and setter) for a variable in a class, if they do not exist.

Usage:
| transformation |
transformation := (RBAddVariableAccessorTransformation
	variable: 'variableName'
	class: #RBVariableTransformation
	classVariable: false)
	transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- the variable with which the accessors will be created shall exist. The parameter isClassVariable indicates whether to look in the instance or class variables.
"
Class {
	#name : #RBAddVariableAccessorTransformation,
	#superclass : #RBVariableTransformation,
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #private }
RBAddVariableAccessorTransformation >> createGetterAccessor [

	(self definingClass getterMethodFor: variableName) ifNil: [ self defineGetterMethod ]
]

{ #category : #private }
RBAddVariableAccessorTransformation >> createSetterAccessor [

	(self definingClass setterMethodFor: variableName) ifNil: [ self defineSetterMethod ]
]

{ #category : #private }
RBAddVariableAccessorTransformation >> defineGetterMethod [

	| selector |
	selector := self safeMethodNameFor: self definingClass basedOn: variableName asString.
	self definingClass 
		compile: ('<1s><r><t>^ <2s>' expandMacrosWith: selector with: variableName)
		classified: #(#accessing).
	^ selector
]

{ #category : #private }
RBAddVariableAccessorTransformation >> defineSetterMethod [

	| selector sourceCode |
	sourceCode := '<1s> anObject<r><t><2s> := anObject'.
	selector := self safeMethodNameFor: self definingClass basedOn: variableName asString , ':'.
	self definingClass 
		compile: (sourceCode expandMacrosWith: selector with: variableName)
		classified: #accessing.
	^selector
]

{ #category : #preconditions }
RBAddVariableAccessorTransformation >> preconditions [

	class := self model classObjectFor: className.

	^ isClassVariable 
		ifTrue: [RBCondition definesClassVariable: variableName asSymbol in: class]
		ifFalse: [RBCondition definesInstanceVariable: variableName in: class]
]

{ #category : #executing }
RBAddVariableAccessorTransformation >> privateTransform [

	self
		createGetterAccessor;
		createSetterAccessor
]

{ #category : #private }
RBAddVariableAccessorTransformation >> safeMethodNameFor: aClass basedOn: aString [ 
	"Creates an unused method name containing aString"

	| baseString newString hasParam i |
	baseString := aString copy.
	baseString at: 1 put: baseString first asLowercase.
	newString := baseString.
	hasParam := newString last = $:.
	hasParam 
		ifTrue: [baseString := newString copyFrom: 1 to: newString size - 1].
	i := 0.
	[aClass hierarchyDefinesMethod: newString asSymbol] whileTrue: 
			[i := i + 1.
			newString := baseString , i printString 
						, (hasParam ifTrue: [':'] ifFalse: [''])].
	^newString asSymbol
]
