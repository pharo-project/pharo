"
I am a refactoring for copy a class.

My preconditions verify, that the copied class exists (in  the current namespace) and that the new copy class name is valid and not yet used as a global variable name.

The refactoring transformation create a new class and copy all instance and class methods of copied class.

Example
---------------
```
	(ReCopyClassRefactoring 
		copyClass: #RBFooLintRuleTestData 
		withName: #RBFooLintRuleTestData1 in: #Example1) execute. 
```

## Implementation

- className refers to a <Symbol> representing the new class to be created.
- aClass is the RefactoringBrowser representation of className.

"
Class {
	#name : 'ReDuplicateClassRefactoring',
	#superclass : 'RBClassRefactoring',
	#instVars : [
		'aClass',
		'packageName',
		'classMethods',
		'instanceMethods',
		'superclassName',
		'packageTagName'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'partial instantiation' }
ReDuplicateClassRefactoring class >> copyClass: aClass [

	^ self new
		  initializeWithClass: aClass;
		  yourself
]

{ #category : 'copying' }
ReDuplicateClassRefactoring class >> copyClass: aClass withName: aSymbol [

	^ self new copyClass: aClass withName: aSymbol
]

{ #category : 'copying' }
ReDuplicateClassRefactoring class >> copyClass: aClass withName: copyName in: aSymbol [

	^ (self copyClass: aClass withName: copyName)
		  packageName: aSymbol;
		  yourself
]

{ #category : 'copying' }
ReDuplicateClassRefactoring class >> model: aRBNamespace copyClass: aClass withName: copyName [

	^ (self copyClass: aClass withName: copyName)
		  model: aRBNamespace;
		  yourself
]

{ #category : 'copying' }
ReDuplicateClassRefactoring class >> model: aRBNamespace copyClass: aClass withName: copyName in: aSymbol [

	^ (self copyClass: aClass withName: copyName in: aSymbol)
		  model: aRBNamespace;
		  yourself
]

{ #category : 'preconditions' }
ReDuplicateClassRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition isValidClassName: className).
		  (RBCondition isGlobal: className in: self model) not }
]

{ #category : 'transforming' }
ReDuplicateClassRefactoring >> classMethods [

	^ classMethods
		ifNil: [ classMethods := aClass classSide localMethods ].
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> classMethods: anObject [

	classMethods := anObject
]

{ #category : 'transforming' }
ReDuplicateClassRefactoring >> copyClass [

	self generateChangesFor:
		((RBInsertNewClassRefactoring model: self model className: className)
			 superclass: aClass superclass;
			 traitComposition: aClass traitComposition;
			 packageName: self packageName;
			 comment: aClass comment;
			 tagName: self packageTagName;
			 yourself)
]

{ #category : 'initialize' }
ReDuplicateClassRefactoring >> copyClass: cls withName: aName [

	self className: aName.
	aClass := self classObjectFor: cls
]

{ #category : 'transforming' }
ReDuplicateClassRefactoring >> copyMethods: aCollectionOfMethods of: sourceClass in: newClass [
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
ReDuplicateClassRefactoring >> copySelectedMethods [
	| newClass |
	
	newClass := self model classNamed: className.
	self copyMethods: self instanceMethods of: aClass in: newClass.
	self copyMethods: self classMethods of: aClass classSide in: newClass classSide
]

{ #category : 'transforming' }
ReDuplicateClassRefactoring >> copyVariables [
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

{ #category : 'initialize' }
ReDuplicateClassRefactoring >> initializeWithClass: cls [

	aClass := self classObjectFor: cls
]

{ #category : 'transforming' }
ReDuplicateClassRefactoring >> instanceMethods [

	^ instanceMethods
		ifNil: [ instanceMethods := aClass localMethods ]
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> instanceMethods: anObject [

	instanceMethods := anObject
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> packageName [

	^ packageName ifNil: [ aClass packageName ]
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> packageName: anObject [

	packageName := anObject
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> packageTagName [

	^ packageTagName
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> packageTagName: aString [

	packageTagName := aString
]

{ #category : 'transforming' }
ReDuplicateClassRefactoring >> privateTransform [

	self copyClass.
	self copyVariables.
	self copySelectedMethods.

]

{ #category : 'initialize' }
ReDuplicateClassRefactoring >> sourceClass: classToCopy [

	aClass := self classObjectFor: classToCopy
]

{ #category : 'accessing' }
ReDuplicateClassRefactoring >> superclassName: aClassName [ 

	superclassName := aClassName
]
