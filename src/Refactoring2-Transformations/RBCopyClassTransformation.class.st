"
I am a transformation for copying a class.

The refactoring transformation creates a new class and copies all instance and class methods of copied class.

Usage:
| transformation |
transformation := (RBCopyClassTransformation
	copyClass: #FooTest
	withName: #FooTestCopy)
	transform.
(ChangesBrowser changes: transformation model changes changes) open

Preconditions:
- copied class exists
- new name is valid and not used as a global variable name
"
Class {
	#name : #RBCopyClassTransformation,
	#superclass : #RBClassTransformation,
	#instVars : [
		'aClass',
		'category'
	],
	#category : #'Refactoring2-Transformations-Model'
}

{ #category : #copying }
RBCopyClassTransformation class >> copyClass: cls withName: aSymbol [
	^ self new copyClass: cls withName: aSymbol
]

{ #category : #copying }
RBCopyClassTransformation class >> copyClass: cls withName: copyName in: aSymbol [
	^ self new
		category: aSymbol;
		copyClass: cls withName: copyName
]

{ #category : #copying }
RBCopyClassTransformation class >> model: aRBSmalltalk copyClass: cls withName: aSymbol [
	^ self new
		model: aRBSmalltalk;
		copyClass: cls withName: aSymbol
]

{ #category : #copying }
RBCopyClassTransformation class >> model: aRBSmalltalk copyClass: cls withName: copyName in: aSymbol [
	^ self new
		model: aRBSmalltalk;
		category: aSymbol;
		copyClass: cls withName: copyName
]

{ #category : #accessing }
RBCopyClassTransformation >> category [

	^ category ifNil: [ category := aClass category ]
]

{ #category : #accessing }
RBCopyClassTransformation >> category: aSymbol [
	category := aSymbol 
]

{ #category : #accessing }
RBCopyClassTransformation >> copyClass [
	(RBAddClassTransformation 
	model: self model
	addClass: className
	superclass: aClass superclass asString  
	subclasses: #()
	category: self category)
	transform.
	
]

{ #category : #accessing }
RBCopyClassTransformation >> copyClass: cls withName: aName [
	self className: aName.
	aClass := self model classObjectFor: cls.
]

{ #category : #accessing }
RBCopyClassTransformation >> copyMethods [
	| newClass |
	newClass := (self model classNamed: className).
	self copyMethodsOf: aClass in: newClass.
	self copyMethodsOf: aClass classSide in: newClass classSide
]

{ #category : #accessing }
RBCopyClassTransformation >> copyMethodsOf: rbClass1 in: rbClass2 [
| transformation |
	rbClass1 selectors do: [ :symbol | | rbMethod |
		rbMethod := rbClass1 methodFor: symbol.
		transformation := (RBAddMethodTransformation 
		model:self model
		sourceCode: rbMethod source
		in: rbClass2
		withProtocols: rbMethod protocols)
		transform.
	]
]

{ #category : #accessing }
RBCopyClassTransformation >> copyVariables [
| transformation |
	aClass instanceVariableNames do: [ :varName |
		transformation:= (RBAddVariableTransformation 
		model: self model 
		instanceVariable: varName
		class: className)
		transform].

	aClass classVariableNames do: [ :varName |
		transformation:= (RBAddVariableTransformation 
		model: self model
		classVariable: varName
		class: className)
		transform. ]
]

{ #category : #accessing }
RBCopyClassTransformation >> preconditions [ 
	^ (RBCondition isValidClassName: className) 
		& (RBCondition isGlobal: className in: self model) not
]

{ #category : #accessing }
RBCopyClassTransformation >> privateTransform [
	self copyClass.
	self copyVariables.
	self copyMethods.
]
