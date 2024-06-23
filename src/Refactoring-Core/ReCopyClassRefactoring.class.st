"
I am a refactoring for copy a class.

My preconditions verify, that the copied class exists (in  the current namespace) and that the new copy class name is valid and not yet used as a global variable name 

The refactoring transformation create a new class and copy all instance and class methods of copied class.

# Example
```smalltalk
	(ReCopyClassRefactoring 
		copyClass: #RBFooLintRuleTestData 
		withName: #RBFooLintRuleTestData1 in: #Example1) execute. 
```
"
Class {
	#name : 'ReCopyClassRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'aClass',
		'packageName'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'copying' }
ReCopyClassRefactoring class >> copyClass: aClass withName: aSymbol [

	^ self new copyClass: aClass withName: aSymbol
]

{ #category : 'copying' }
ReCopyClassRefactoring class >> copyClass: aClass withName: copyName in: aSymbol [

	^ (self copyClass: aClass withName: copyName)
		  packageName: aSymbol;
		  yourself
]

{ #category : 'copying' }
ReCopyClassRefactoring class >> model: aRBNamespace copyClass: aClass withName: copyName [

	^ (self copyClass: aClass withName: copyName)
		  model: aRBNamespace;
		  yourself
]

{ #category : 'copying' }
ReCopyClassRefactoring class >> model: aRBNamespace copyClass: aClass withName: copyName in: aSymbol [

	^ (self copyClass: aClass withName: copyName in: aSymbol)
		  model: aRBNamespace;
		  yourself
]

{ #category : 'preconditions' }
ReCopyClassRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isValidClassName: className).
		  (RBCondition isGlobal: className in: self model) not }
]

{ #category : 'transforming' }
ReCopyClassRefactoring >> copyClass [

	self generateChangesFor: ((RBInsertNewClassRefactoring model: self model className: className)
			 superclass: aClass superclass;
			 packageName: self packageName;
			 comment: aClass comment;
			 yourself)
]

{ #category : 'copying' }
ReCopyClassRefactoring >> copyClass: cls withName: aName [
	self className: aName.
	aClass := self classObjectFor: cls
]

{ #category : 'transforming' }
ReCopyClassRefactoring >> copyMethods [
	| newClass |
	newClass := (self model classNamed: className).
	self copyMethodsOf: aClass in: newClass.
	self copyMethodsOf: aClass classSide in: newClass classSide
]

{ #category : 'copying' }
ReCopyClassRefactoring >> copyMethodsOf: rbClass1 in: rbClass2 [
	rbClass1 selectors do: [ :symbol | | rbMethod |
		rbMethod := rbClass1 methodFor: symbol.
		self generateChangesFor:
			(RBAddMethodTransformation
				model: self model
				sourceCode: rbMethod source
				in: rbClass2
				withProtocol: rbMethod protocols)
	]
]

{ #category : 'transforming' }
ReCopyClassRefactoring >> copyVariables [
	aClass instanceVariableNames do: [ :varName |
		self generateChangesFor: (RBAddInstanceVariableRefactoring
		model: self model
		variable: varName
		class: className) ].

	aClass classVariableNames do: [ :varName |
		self generateChangesFor: (RBAddClassVariableRefactoring
		model: self model
		variable: varName
		class: className) ]
]

{ #category : 'accessing' }
ReCopyClassRefactoring >> packageName [

	^ packageName ifNil: [ aClass packageName ]
]

{ #category : 'accessing' }
ReCopyClassRefactoring >> packageName: anObject [

	packageName := anObject
]

{ #category : 'transforming' }
ReCopyClassRefactoring >> privateTransform [
	self copyClass.
	self copyVariables.
	self copyMethods
]
