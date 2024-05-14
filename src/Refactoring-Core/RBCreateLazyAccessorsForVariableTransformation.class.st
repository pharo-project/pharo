"
I am a refactoring for creating accessors with lazy initialization for variables.
The getter/setter are only created if they do not already exist in the class or its subclasses.

My precondition is that the variable name is defined for this class.

### Example

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
	#name : 'RBCreateLazyAccessorsForVariableTransformation',
	#superclass : 'RBCreateAccessorsForVariableTransformation',
	#instVars : [
		'defaultValue'
	],
	#category : 'Refactoring-Core-Transformation',
	#package : 'Refactoring-Core',
	#tag : 'Transformation'
}

{ #category : 'displaying' }
RBCreateLazyAccessorsForVariableTransformation class >> basicMenuItemString [

	^ 'Generate lazy accessors'
]

{ #category : 'instance creation' }
RBCreateLazyAccessorsForVariableTransformation class >> model: aRBSmalltalk variable: aVarName class: aClass classVariable: aBoolean defaultValue: aString [
	^(self
		model: aRBSmalltalk
		variable: aVarName
		class: aClass)
		classVariable: aBoolean;
		defaultValue: aString;
		yourself
]

{ #category : 'instance creation' }
RBCreateLazyAccessorsForVariableTransformation class >> variable: aVarName class: aClass classVariable: aBoolean defaultValue: aString [

	^(self variable: aVarName class: aClass)
		classVariable: aBoolean;
		defaultValue: aString;
		yourself
]

{ #category : 'preconditions' }
RBCreateLazyAccessorsForVariableTransformation >> applicabilityPreconditions [ 
	"In addition check that the initialization is valid"

	^ super applicabilityPreconditions ,
		{ (RBCondition withBlock: [  self verifyInitializationExpressionOf: self defaultValue. true ]) }
]

{ #category : 'preconditions' }
RBCreateLazyAccessorsForVariableTransformation >> checkVariableReferencesIn: aParseTree [

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
RBCreateLazyAccessorsForVariableTransformation >> createGetterAccessor [

	getterMethodName := self findGetterMethod.
	getterMethodName ifNil: [ getterMethodName := self defineGetterMethod ]
		ifNotNil: [ |matcher|
			matcher := self parseTreeSearcherClass new
			matchesMethod: '`method ^' , variableName
			do: [:aNode :ans | aNode selector].
			(self checkClass: self definingClass selector: getterMethodName using: matcher)
				ifNotNil: [ selector := getterMethodName ].
			getterMethodName := self defineGetterMethod ]
]

{ #category : 'accessing' }
RBCreateLazyAccessorsForVariableTransformation >> defaultValue [
	^ defaultValue ifNil: [ defaultValue := 'nil' ]
]

{ #category : 'accessing' }
RBCreateLazyAccessorsForVariableTransformation >> defaultValue: aString [
	defaultValue := aString
]

{ #category : 'transforming' }
RBCreateLazyAccessorsForVariableTransformation >> defineGetterMethod [

	self generateChangesFor:
		(RBAddMethodTransformation
			 sourceCode: ('<1s><r><r><t>^ <2s> ifNil: [ <2s> := <3s> ]'
					  expandMacrosWith: self selector
					  with: variableName
					  with: self defaultValue)
			 in: self definingClass
			 withProtocol: #accessing).

	^ selector
]

{ #category : 'preconditions' }
RBCreateLazyAccessorsForVariableTransformation >> verifyInitializationExpressionOf: initializer [
	| tree |
	tree := self parserClass
		parseExpression: initializer
		onError: [ :msg :index | self refactoringError: 'Illegal initialization code because: ', msg ].
	tree isValue
		ifFalse: [ self refactoringError: 'The initialization code cannot be a return node or a list of statements' ].
	self checkVariableReferencesIn: tree
]
