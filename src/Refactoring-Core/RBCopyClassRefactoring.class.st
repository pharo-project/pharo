"
I am a refactoring for copy a class.

My preconditions verify, that the copied class exists (in  the current namespace) and that the new copy class name is valid and not yet used as a global variable name.

The refactoring transformation create a new class and copy all instance and class methods of copied class.

Example
---------------
```
	(RBCopyClassRefactoring 
		copyClass: #RBFooLintRuleTestData 
		withName: #RBFooLintRuleTestData1 in: #Example1) execute. 
```

## Implementation

- className refers to a <Symbol> representing the new class to be created.
- aClass is the RefactoringBrowser representation of className.

"
Class {
	#name : 'RBCopyClassRefactoring',
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
RBCopyClassRefactoring class >> copyClass: aClass withName: aSymbol [

	^ self new copyClass: aClass withName: aSymbol
]

{ #category : 'copying' }
RBCopyClassRefactoring class >> copyClass: aClass withName: copyName in: aSymbol [

	^ (self copyClass: aClass withName: copyName)
		  packageName: aSymbol;
		  yourself
]

{ #category : 'copying' }
RBCopyClassRefactoring class >> model: aRBNamespace copyClass: aClass withName: copyName [

	^ (self copyClass: aClass withName: copyName)
		  model: aRBNamespace;
		  yourself
]

{ #category : 'copying' }
RBCopyClassRefactoring class >> model: aRBNamespace copyClass: aClass withName: copyName in: aSymbol [

	^ (self copyClass: aClass withName: copyName in: aSymbol)
		  model: aRBNamespace;
		  yourself
]

{ #category : 'preconditions' }
RBCopyClassRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isValidClassName: className).
		  (RBCondition isGlobal: className in: self model) not }
]

{ #category : 'transforming' }
ReCopyClassRefactoring >> classMethods [

	^ classMethods
		ifNil: [ classMethods := aClass classSide allMethods ].
]

{ #category : 'accessing' }
ReCopyClassRefactoring >> classMethods: anObject [

	classMethods := anObject
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
ReCopyClassRefactoring >> copyMethods: aCollectionOfMethods of: sourceClass in: newClass [
	"Copy aCollectionOfMethods from sourceClass to newClass "

	aCollectionOfMethods do: [ :rbMethod | 
		self generateChangesFor:
			(RBAddMethodTransformation
				model: self model
				sourceCode: rbMethod source
				in: newClass
				withProtocol: rbMethod protocols) ]
]

{ #category : 'copying' }
RBCopyClassRefactoring >> copyMethodsOf: rbClass1 in: rbClass2 [
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
RBCopyClassRefactoring >> copyVariables [
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

{ #category : 'transforming' }
ReCopyClassRefactoring >> instanceMethods [

	^ instanceMethods
		ifNil: [ instanceMethods := aClass methods ]
]

{ #category : 'accessing' }
RBCopyClassRefactoring >> packageName [

	^ packageName ifNil: [ aClass packageName ]
]

{ #category : 'accessing' }
RBCopyClassRefactoring >> packageName: anObject [

	packageName := anObject
]

{ #category : 'transforming' }
RBCopyClassRefactoring >> privateTransform [
	self copyClass.
	self copyVariables.
	self copyMethods
]
