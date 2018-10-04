Extension { #name : #RBParseTreeRewriter }

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeRewriter class >> buildMessageSendWith: aSelector and: argsAndValues needsReturn: aBoolean [

	^ String streamContents: [ :string |
		aBoolean ifTrue: [ string nextPutAll: '^ ' ].
		
		string nextPutAll: 'self '.
		argsAndValues
			ifEmpty: [ string nextPutAll: aSelector asString ] 
			ifNotEmpty: [ 
				aSelector keywords with: argsAndValues associations
					do: [ :key :arg | 
						string nextPutAll: key asString; nextPut: $ .
						string nextPutAll: (argsAndValues at: arg key).
						string nextPut: $ ] ] ]
]

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeRewriter class >> mapArguments: arguments toValues: values in: aTree [

	^ (arguments size = values size)
		"map directly"
		ifTrue: [
			| argsAndValues |
			argsAndValues := OrderedCollection new.
			arguments with: values do: [ :arg :val | 
				argsAndValues add: (arg name asString -> val value storeString) ].
			argsAndValues ]
		"open browser"
		ifFalse: [
			(RBMethodArgumentsSelector
				openOn: (arguments collect: [:e | e name asString])
				and: (values collect: [ :e | e value storeString ])
				in: aTree)
				argumentsAndValues ]
]

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeRewriter class >> replaceCode: subtree byMessageSendTo: aMethod in: aCompiledMethod [

	| argsAndValues newSourceCode |
	argsAndValues := self mapArguments: aMethod ast arguments
								toValues: (subtree allChildren select: #isLiteralNode)
								in: subtree.
	newSourceCode := self buildMessageSendWith: aMethod selector 
			and: argsAndValues asDictionary
			needsReturn: subtree isReturn.
		
	^ RBReplaceSubtreeTransformation
		replace: subtree sourceCode
		to: newSourceCode
		inMethod: aCompiledMethod selector
		inClass: aCompiledMethod methodClass
]

{ #category : #'*Refactoring2-Transformations' }
RBParseTreeRewriter class >> replaceStatements: code with: newCode in: aParseTree [

	| tree replaceStmt |
	tree := self buildTree: code method: false.
	tree isSequence 
		ifFalse: [tree := RBSequenceNode statements: (Array with: tree)].
	tree temporaries: (Array with: (RBPatternVariableNode named: '`@temps')).
	tree addNodeFirst: (RBPatternVariableNode named: '`@.S1').
	tree lastIsReturn ifTrue: [replaceStmt := '| `@temps | `@.S1. ^' , newCode].
		
	replaceStmt := '| `@temps | `@.S1. ' , newCode.
	(tree statements last = aParseTree body statements last)
		ifFalse: 
			[tree addNode: (RBPatternVariableNode named: '`@.S2').
			replaceStmt := replaceStmt, '. `@.S2'].
	
	^self 
		replace: tree formattedCode
		with: replaceStmt
		in: aParseTree
		onInterval: aParseTree sourceInterval
]
