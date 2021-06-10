"
I am a refactoring for creating accessors with lazy initialization for variables.

I am used by a couple of other refactorings creating new variables and accessors.

My precondition is that the variable name is defined for this class.

Example
--------
Script
```
(RBCreateAccessorsWithLazyInitializationForVariableRefactoring 
	variable: 'foo1' 
	class: RBLintRuleTestData 
	classVariable: false 
	defaultValue: '123') execute
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
	#name : #RBCreateAccessorsWithLazyInitializationForVariableRefactoring,
	#superclass : #RBCreateAccessorsForVariableRefactoring,
	#instVars : [
		'defaultValue'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring class >> model: aRBSmalltalk variable: aVarName class: aClass classVariable: aBoolean defaultValue: aString [
	^(self 
		model: aRBSmalltalk
		variable: aVarName
		class: aClass)
		classVariable: aBoolean;
		defaultValue: aString;
		yourself
]

{ #category : #'instance creation' }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring class >> variable: aVarName class: aClass classVariable: aBoolean defaultValue: aString [

	^(self variable: aVarName class: aClass)
		classVariable: aBoolean;
		defaultValue: aString;
		yourself
]

{ #category : #preconditions }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> checkVariableReferencesIn: aParseTree [

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

{ #category : #private }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> createGetterAccessor [

	getterMethod := self findGetterMethod.
	getterMethod ifNil: [ getterMethod := self defineGetterMethod ]
		ifNotNil: [ |matcher|
			matcher := self parseTreeSearcherClass new
			matchesMethod: '`method ^' , variableName 
			do: [:aNode :ans | aNode selector].
			(self checkClass: self definingClass selector: getterMethod using: matcher)
				ifNotNil: [ selector := getterMethod ].
			getterMethod := self defineGetterMethod ]
]

{ #category : #accessing }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> defaultValue [
	^ defaultValue ifNil: [ defaultValue := 'nil' ]
]

{ #category : #accessing }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> defaultValue: aString [
	defaultValue := aString
]

{ #category : #transforming }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> defineGetterMethod [
	
	self definingClass 
		compile: ('<1s><r><r><t>^ <2s> ifNil: [ <2s> := <3s> ]' 
			expandMacrosWith: self selector 
			with: variableName
			with: self defaultValue)
		classified: #(#accessing).
	^selector
]

{ #category : #preconditions }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> preconditions [
	^ super preconditions & 
	(RBCondition withBlock: [ 
		self verifyInitializationExpressionOf: self defaultValue.
		true ])
]

{ #category : #preconditions }
RBCreateAccessorsWithLazyInitializationForVariableRefactoring >> verifyInitializationExpressionOf: initializer [
	| tree |
	tree := self parserClass
		parseExpression: initializer
		onError: [ :msg :index | self refactoringFailure: 'Illegal initialization code because:.', msg ].
	tree isValue
		ifFalse: [ self refactoringFailure: 'The initialization code cannot be a return node or a list of statements' ].
	self checkVariableReferencesIn: tree.
]
