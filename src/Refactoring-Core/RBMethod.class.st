"
I represent a method for the refactoring framework.

I am used by RBClass / RBMetaclass resp. RBAbstractClass for methods created or changed during a refactoring operation.
I represent the method with a selector, source and if I am created from an existing method, its 
CompiledMethod. 
I know my method class (a RBClass or RBMetaclass). 
You should not directly create instances of me but query or create a method from a RBClass.

I only implement a small part of  CompiledMethod interface, that is used for refactoring operations, like querying symbols, literals or the whole method source.

"
Class {
	#name : 'RBMethod',
	#superclass : 'RBEntity',
	#instVars : [
		'compiledMethod',
		'class',
		'selector',
		'source',
		'isFromTrait'
	],
	#category : 'Refactoring-Core-Model',
	#package : 'Refactoring-Core',
	#tag : 'Model'
}

{ #category : 'instance creation' }
RBMethod class >> for: aRBClass fromMethod: aCompiledMethod andSelector: aSymbol [

	^ self new
		  modelClass: aRBClass;
		  compiledMethod: aCompiledMethod;
		  selector: aSymbol;
		  yourself
]

{ #category : 'instance creation' }
RBMethod class >> for: aRBClass source: aString selector: aSelector [
	^(self new)
		modelClass: aRBClass;
		selector: aSelector;
		source: aString;
		yourself
]

{ #category : 'accessing' }
RBMethod >> argumentNames [
	"Return the arguments of the receiver, aMethod."
	
	^ self ast argumentNames
]

{ #category : 'accessing' }
RBMethod >> ast [
	"Returns a semantical AST based on the source code of the receiver, aMethod."
	
	^ self parseTree
]

{ #category : 'compiling' }
RBMethod >> compileTree: aMethodNode [
	"Define the method specified by the AST aMethodNode."
	| method sourceCode change |
	sourceCode := aMethodNode newSource.
	change := self modelClass model
		compile: sourceCode
		in: self modelClass
		classified: self protocols.
	method := self class
		for: self modelClass
		source: sourceCode
		selector: aMethodNode selector.
	self modelClass addMethod: method.
	^ change
]

{ #category : 'accessing - implementation' }
RBMethod >> compiledMethod [
	^compiledMethod
]

{ #category : 'accessing - implementation' }
RBMethod >> compiledMethod: aCompiledMethod [
	compiledMethod := aCompiledMethod.
	self isFromTrait: compiledMethod isFromTrait
]

{ #category : 'accessing' }
RBMethod >> isFromTrait [
	"Returns whether the receiver, a Method, is defined in a trait."
	
	^ isFromTrait ifNil: [ isFromTrait := false ]
]

{ #category : 'accessing' }
RBMethod >> isFromTrait: anObject [
	isFromTrait := anObject
]

{ #category : 'private' }
RBMethod >> literal: anObject containsReferenceTo: aSymbol [
	anObject = aSymbol
		ifTrue: [ ^ true ].
	anObject class = Array
		ifFalse: [ ^ false ].
	^ anObject anySatisfy: [ :each | self literal: each containsReferenceTo: aSymbol ]
]

{ #category : 'accessing' }
RBMethod >> method [
	self deprecated: 'Use compiledMethod' transformWith: '`@rec method' -> '`@rec compiledMethod'.
	^compiledMethod
]

{ #category : 'accessing' }
RBMethod >> method: aCompiledMethod [
	self deprecated: 'Use compiledMethod' transformWith: '`@rec method: `@arg' -> '`@rec compiledMethod: `@arg'.
	
	self compiledMethod: aCompiledMethod
]

{ #category : 'accessing' }
RBMethod >> methodClass [

	^ class
]

{ #category : 'accessing' }
RBMethod >> modelClass [
	^class
]

{ #category : 'accessing' }
RBMethod >> modelClass: aRBClass [
	class := aRBClass
]

{ #category : 'accessing' }
RBMethod >> origin [
	"Returns the origin of the receiver, aMethod. The origin can be a class or a trait."
	
	^ class
]

{ #category : 'accessing' }
RBMethod >> package [
	"Return the package name of the receiver, aMethod."
	compiledMethod ifNotNil: [ ^ compiledMethod package name ].
	^ class packageName
]

{ #category : 'accessing' }
RBMethod >> parseTree [
	"Returns a semantical AST based on the source code of the receiver, aMethod."

	| tree |
	tree := self parserClass
		  parseMethod: self source
		  onError: [ :str :pos | ^ nil ].
	tree ifNotNil: [ tree doSemanticAnalysis ].
	^ tree
]

{ #category : 'private' }
RBMethod >> parseTreeSearcherClass [
	^ OCParseTreeSearcher
]

{ #category : 'private' }
RBMethod >> parserTreeSearcher [
	^ self parseTreeSearcherClass new
]

{ #category : 'printing' }
RBMethod >> printOn: aStream [
	class printOn: aStream.
	aStream
		nextPutAll: '>>';
		nextPutAll: self selector
]

{ #category : 'accessing' }
RBMethod >> protocols [
	^ self modelClass protocolsFor: self selector
]

{ #category : 'cross reference testing' }
RBMethod >> refersToClassNamed: aSymbol [
	"Returns whether the receiver, a method, refers to the class named aSymbol."
	
	| searcher |
	searcher := self parserTreeSearcher.
	searcher matches: aSymbol asString do: [ :node :answer | true ].
	^ (searcher executeTree: self parseTree initialAnswer: false)
		or: [ self refersToSymbol: aSymbol ]
]

{ #category : 'cross reference testing' }
RBMethod >> refersToSymbol: aSymbol [
	"Returns whether the receiver, a Method, refers to the symbol, aSymbol."
	
	| searcher |
	searcher := self parserTreeSearcher.
	searcher
		matches: aSymbol printString do: [ :node :answer | true ];
		matches: '`#literal'
			do:
				[ :node :answer | answer or: [ self literal: node value containsReferenceTo: aSymbol ] ].
	(OCScanner isSelector: aSymbol)
		ifTrue: [ searcher
				matches:
					'`@object '
						, (self parseTreeSearcherClass buildSelectorString: aSymbol)
				do: [ :node :answer | true ] ].
	^ searcher executeTree: self parseTree initialAnswer: false
]

{ #category : 'cross reference testing' }
RBMethod >> refersToVariable: aString [
	"Returns whether the receiver, aMethod, refers to the variable named aString."
	
	| searcher tree |
	tree := self parseTree.
	((tree defines: aString) or: [ tree body defines: aString ])
		ifTrue: [ ^ false ].
	searcher := self parserTreeSearcher.
	searcher
		matches: aString do: [ :node :answer | true ];
		matches: '[:`@vars | | `@temps | `@.Stmts]'
			do: [ :node :answer |
			answer
				or: [ ((node defines: aString) or: [ node body defines: aString ]) not
						and: [ searcher executeTree: node body initialAnswer: false ] ] ].
	^ searcher executeTree: self parseTree initialAnswer: false
]

{ #category : 'accessing' }
RBMethod >> selector [
	^ selector
]

{ #category : 'accessing' }
RBMethod >> selector: aSymbol [
	selector := aSymbol
]

{ #category : 'accessing' }
RBMethod >> source [
	"Returns the source code of the method receiver."
	
	^ source ifNil: [ source := (class realClass sourceCodeAt: selector) asString ]
]

{ #category : 'accessing' }
RBMethod >> source: aString [
	source := aString
]
