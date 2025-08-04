"
I am a transformation to create accessors with lazy initialization for variables.

My precondition is that the variable name is defined for this class.

Example
--------
Script
```
| transformation |
transformation := (RBAddVariableAccessorWithLazyInitializationTransformation 
	variable: 'foo1' 
	class: RBLintRuleTestData 
	classVariable: false 
	defaultValue: '123') generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

After refactoring we get:
```
RBLintRuleTestData >> foo1 
	^ foo1 ifNil: [foo1 := 123]
	
RBLintRuleTestData >> foo1: anObject
	foo1 := anObject
```
"
Class {
	#name : 'RBAddVariableAccessorWithLazyInitializationTransformation',
	#superclass : 'RBAddVariableAccessorTransformation',
	#instVars : [
		'defaultValue'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'api' }
RBAddVariableAccessorWithLazyInitializationTransformation class >> model: aRBSmalltalk variable: aVarName class: aClass classVariable: aBoolean defaultValue: aString [

	^(self model: aRBSmalltalk variable: aVarName class: aClass classVariable: aBoolean)
		defaultValue: aString;
		yourself
]

{ #category : 'api' }
RBAddVariableAccessorWithLazyInitializationTransformation class >> variable: aVarName class: aClass classVariable: aBoolean defaultValue: aString [

	^(self variable: aVarName class: aClass classVariable: aBoolean)
		defaultValue: aString;
		yourself
]

{ #category : 'preconditions' }
RBAddVariableAccessorWithLazyInitializationTransformation >> applicabilityPreconditions [

	^ super applicabilityPreconditions ,
		  { (RBCondition withBlock: [
				   self verifyInitializationExpressionOf: self defaultValue.
				   true ]) }
]

{ #category : 'preconditions' }
RBAddVariableAccessorWithLazyInitializationTransformation >> checkVariableReferencesIn: aParseTree [

	| searcher |

	searcher := self parseTreeSearcher.
	searcher
		matches: '`var'
		do: [ :aNode :answer |
			| name |

			name := aNode name.
			( aNode whoDefines: name ) ifNil: [ self canReferenceVariable: name in: class  ]
			].
	searcher executeTree: aParseTree
]

{ #category : 'private' }
RBAddVariableAccessorWithLazyInitializationTransformation >> createGetterAccessor [

	(self definingClass getterMethodFor: variableName)
		ifNil: [ self defineGetterMethod ]
		ifNotNil: [ :symbol | self defineGetterCalled: symbol ]
]

{ #category : 'accessing' }
RBAddVariableAccessorWithLazyInitializationTransformation >> defaultValue [
	^ defaultValue ifNil: [ defaultValue := 'nil' ]
]

{ #category : 'accessing' }
RBAddVariableAccessorWithLazyInitializationTransformation >> defaultValue: aString [
	defaultValue := aString
]

{ #category : 'transforming' }
RBAddVariableAccessorWithLazyInitializationTransformation >> defineGetterCalled: aSymbol [

	| selector |
	selector := aSymbol.
	(RBAddMethodTransformation
		model: self model
		sourceCode: ('<1s><r><r><t>^ <2s> ifNil: [ <2s> := <3s> ]'
			expandMacrosWith: selector
			with: variableName
			with: self defaultValue)
		in: self definingClass
		withProtocol: 'accessing') generateChanges.
	^ selector
]

{ #category : 'transforming' }
RBAddVariableAccessorWithLazyInitializationTransformation >> defineGetterMethod [

	^ self defineGetterCalled: self getterMethod
]

{ #category : 'accessing' }
RBAddVariableAccessorWithLazyInitializationTransformation >> getterMethod [
	^ self safeMethodNameFor: self definingClass basedOn: variableName asString
]

{ #category : 'storing' }
RBAddVariableAccessorWithLazyInitializationTransformation >> storeOn: aStream [

	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' variable: '.
	variableName storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream nextPutAll: ' classVariable: '.
	isClassVariable storeOn: aStream.
	aStream nextPutAll: ' defaultValue: '.
	defaultValue storeOn: aStream.
	aStream nextPut: $)
]

{ #category : 'preconditions' }
RBAddVariableAccessorWithLazyInitializationTransformation >> verifyInitializationExpressionOf: initializer [
	| tree |
	tree := self parserClass
		parseExpression: initializer
		onError: [ :msg :index | self refactoringError: 'Illegal initialization code because:.', msg ].
	tree isValue
		ifFalse: [ self refactoringError: 'The initialization code cannot be a return node or a list of statements' ].
	self checkVariableReferencesIn: tree
]
