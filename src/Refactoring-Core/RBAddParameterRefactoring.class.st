"
I am a refactoring operations for adding method arguments.

You can modify the method name and add an additional keyword argument and the default value used by senders of the original method. Only one new argument can be added. But you can change the whole method name, as long as the number of argument matches.

For example, for #r:g:b:  add another parameter ""a"" the new method is
#r:g:b:a: 
or change the whole method to 
#setRed:green:blue:alpha:

This refactoring will add a new method with the new argument, remove the old method (for all implementors) and replace every sender of the prior method with the new one, using the specified default argument.
"
Class {
	#name : #RBAddParameterRefactoring,
	#superclass : #RBChangeMethodNameRefactoring,
	#instVars : [
		'initializer',
		'senders'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBAddParameterRefactoring class >> addParameterToMethod: aSelector in: aClass newSelector: newSelector initializer: init [ 
	^self new addParameterToMethod: aSelector
		in: aClass
		newSelector: newSelector
		initializer: init
]

{ #category : #'instance creation' }
RBAddParameterRefactoring class >> model: aRBSmalltalk addParameterToMethod: aSelector in: aClass newSelector: newSelector initializer: init [ 
	^(self new)
		model: aRBSmalltalk;
		addParameterToMethod: aSelector
			in: aClass
			newSelector: newSelector
			initializer: init;
		yourself
]

{ #category : #initialization }
RBAddParameterRefactoring >> addParameterToMethod: aSelector in: aClass newSelector: newSel initializer: init [ 
	self 
		renameMethod: aSelector
		in: aClass
		to: newSel
		permutation: (1 to: newSel numArgs).
	initializer := init
]

{ #category : #preconditions }
RBAddParameterRefactoring >> checkSendersAccessTo: name [

	(#('self' 'super') includes: name) ifTrue: [ ^ self ].
	self senders
		detect: [ :each | (self canReferenceVariable: name in: each) not ]
		ifFound: [ :violatorClass | 
			self
				refactoringError:
					('<1s> doesn''t appear to be defined in <2p>'
						expandMacrosWith: name
						with: violatorClass) ]
]

{ #category : #preconditions }
RBAddParameterRefactoring >> checkVariableReferencesIn: aParseTree [

	| searcher |

	searcher := self parseTreeSearcher.
	searcher
		matches: '`var'
		do: [ :aNode :answer | 
			| name |

			name := aNode name.
			( aNode whoDefines: name ) ifNil: [ self checkSendersAccessTo: name ]
			].
	searcher executeTree: aParseTree
]

{ #category : #private }
RBAddParameterRefactoring >> modifyImplementorParseTree: parseTree in: aClass [ 
	| name newArg allTempVars |
	allTempVars := parseTree allDefinedVariables.
	name := self safeVariableNameFor: aClass temporaries: allTempVars.
	newArg := RBVariableNode named: name.
	parseTree 
		renameSelector: newSelector
		andArguments: parseTree arguments , (Array with: newArg)
]

{ #category : #preconditions }
RBAddParameterRefactoring >> myConditions [
	^RBCondition withBlock: 
			[oldSelector numArgs + 1 = newSelector numArgs 
				ifFalse: 
					[self refactoringFailure: newSelector printString 
								, ' doesn''t have the proper number of arguments.'].
			self verifyInitializationExpression.
			true]
]

{ #category : #private }
RBAddParameterRefactoring >> newSelectorString [
	| stream keywords |
	stream := WriteStream on: String new.
	keywords := newSelector keywords.
	1 to: keywords size
		do: 
			[:i | 
			stream nextPutAll: (keywords at: i).
			i == keywords size 
				ifTrue: 
					[stream
						nextPut: $(;
						nextPutAll: initializer;
						nextPut: $)]
				ifFalse: 
					[stream
						nextPutAll: ' ``@arg';
						nextPutAll: i printString].
			stream nextPut: $ ].
	^stream contents
]

{ #category : #private }
RBAddParameterRefactoring >> parseTreeRewriter [
	| rewriteRule oldString newString |
	rewriteRule := RBParseTreeRewriter new.
	oldString := self buildSelectorString: oldSelector.
	newString := self newSelectorString.
	rewriteRule replace: '``@object ' , oldString
		with: '``@object ' , newString.
	^rewriteRule
]

{ #category : #private }
RBAddParameterRefactoring >> safeVariableNameFor: aClass temporaries: allTempVars [ 
	| baseString i newString |
	newString := baseString := 'anObject'.
	i := 0.
	
	[(allTempVars includes: newString) 
		or: [aClass definesInstanceVariable: newString]] 
			whileTrue: 
				[i := i + 1.
				newString := baseString , i printString].
	^newString
]

{ #category : #private }
RBAddParameterRefactoring >> senders [

	senders
		ifNil: [ senders := Set new.
			self model allReferencesTo: oldSelector do: [ :each | senders add: each modelClass ]
			].
	^ senders
]

{ #category : #printing }
RBAddParameterRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' addParameterToMethod: #';
		nextPutAll: oldSelector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' newSelector: #';
		nextPutAll: newSelector;
		nextPutAll: ' initializer: ''';
		nextPutAll: initializer;
		nextPutAll: ''')'
]

{ #category : #preconditions }
RBAddParameterRefactoring >> verifyInitializationExpression [
	| tree |
	tree := self parserClass
		parseExpression: initializer
		onError:
			[ :msg :index | self refactoringFailure: 'Illegal initialization code because:.' , msg ].
	tree isValue
		ifFalse: [ self
				refactoringFailure:
					'The initialization code cannot be a return node or a list of statements' ].
	self checkVariableReferencesIn: tree
]
