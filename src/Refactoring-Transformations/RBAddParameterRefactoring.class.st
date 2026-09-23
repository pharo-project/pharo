"
I am a refactoring operations for adding method arguments.

You can modify the method name and add an additional keyword argument and the default value used by senders of the original method. Only one new argument can be added. But you can change the whole method name, as long as the number of argument matches.

For example, for `r:g:b:`  add another parameter ""a"" the new method is `r:g:b:a:`
or change the whole method to `setRed:green:blue:alpha:`

This refactoring will 
- add a new method with the new argument, 
- remove the old method (for all implementors) and 
- replace every sender of the prior method with the new one, using the specified default argument.
"
Class {
	#name : 'RBAddParameterRefactoring',
	#superclass : 'RBChangeMethodNameRefactoring',
	#instVars : [
		'newArgs',
		'senders'
	],
	#category : 'Refactoring-Transformations',
	#package : 'Refactoring-Transformations'
}

{ #category : 'instance creation' }
RBAddParameterRefactoring class >> addParameterToMethod: aSelector in: aClass newSelector: newSelector permutation: aColl1 newArgs: aColl2 [
	^ self new
		addParameterToMethod: aSelector
		in: aClass
		newSelector: newSelector
		permutation: aColl1
		newArgs: aColl2
]

{ #category : 'instance creation' }
RBAddParameterRefactoring class >> model: aRBSmalltalk addParameterToMethod: aSelector in: aClass newSelector: newSelector permutation: aColl1 newArgs: aColl2 [
	^ self new
		model: aRBSmalltalk;
		addParameterToMethod: aSelector
		in: aClass
		newSelector: newSelector
		permutation: aColl1
		newArgs: aColl2;
		yourself
]

{ #category : 'initialization' }
RBAddParameterRefactoring >> addParameterToMethod: aSelector in: aClass newSelector: newSel permutation: aColl1 newArgs: aColl2 [
	self
		renameMethod: aSelector
		in: aClass
		to: newSel
		permutation: aColl1.
	newArgs := aColl2
]

{ #category : 'preconditions' }
RBAddParameterRefactoring >> applicabilityPreconditions [

	^ super applicabilityPreconditions, 
	{
		  self newSelectorIsNotDefinedInTargetClassPrecondition.
		  (ReBlockCondition new
			   block: [ oldSelector numArgs <= newSelector numArgs ];
			   violatorErrorString: newSelector printString , ' doesn''t have the proper number of arguments.').
		  (ReBlockCondition new
			   block: [ permutation size = newSelector numArgs ];
			   violatorErrorString: newSelector printString , ' doesn''t have the proper number of permutations.') 
	}, 
	(self newArgs collect: [ :arg |
		ReBlockCondition new
		   block: [ self isValidInitializationExpressionFor: arg ];
		   violatorErrorString: 'Illegal initialization code for argument ' , arg name printString ])
]

{ #category : 'preconditions' }
RBAddParameterRefactoring >> breakingChangePreconditions [

	^ { 
	ReUpToRootDoesNotDefinesMethod new
		classes: self implementors;
		selector: newSelector;
		yourself
	}
]

{ #category : 'preconditions' }
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

{ #category : 'preconditions' }
RBAddParameterRefactoring >> checkVariableReferencesIn: aParseTree [

	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '`var'
		do: [ :aNode :answer |
			| name |
			name := aNode name.
			(aNode whichUpNodeDefines: name) ifNil: [ self checkSendersAccessTo: name ] ].
	searcher executeTree: aParseTree
]

{ #category : 'preconditions' }
RBAddParameterRefactoring >> isValidInitializationExpressionFor: anArgument [

	[
		self verifyInitializationExpressionOf: anArgument argValue.
		^ true ]
	on: RBRefactoringError
	do: [ :e | ^ false ]
]

{ #category : 'private' }
RBAddParameterRefactoring >> modifyImplementorParseTree: parseTree in: aClass [
	| argNames index |
	argNames := newArgs collect: [ :arg | | newArg |
		newArg := self safeVariableNamed: arg newName for: aClass temporaries: (parseTree temporaryNames, parseTree argumentNames).
	index := 0.
		newArg ].
	parseTree
		renameSelector: newSelector
		andArguments: ((permutation
			collect: [ :e | parseTree argumentNames at: e ifAbsent: [ index := index +1.
				argNames at: index	] ])
				collect: [:e | OCVariableNode named: e ]).
	self renameArgumentsIn: parseTree
]

{ #category : 'accessing' }
RBAddParameterRefactoring >> newArgs [
	^ newArgs ifNil: [ newArgs := { } ]
]

{ #category : 'accessing' }
RBAddParameterRefactoring >> newArgs: aCollection [

	newArgs := aCollection
]

{ #category : 'preconditions' }
RBAddParameterRefactoring >> newSelectorIsNotDefinedInTargetClassPrecondition [

	^ ReBlockCondition new
		  block: [ (class directlyDefinesMethod: newSelector) not ];
		  violatorErrorString: newSelector printString , ' is already defined in ' , class name;
		  yourself
]

{ #category : 'action' }
RBAddParameterRefactoring >> renameArgumentsIn: parseTree [
	| newArgNames |
	newArgNames := newArgs collect: [ :arg | arg name ].
	self renameMap do: [ :arg |
		(newArgNames includes: arg name) ifFalse: [
		(self parseTreeRewriterClass rename: arg name to: arg newName) executeTree: parseTree
	] ]
]

{ #category : 'private' }
RBAddParameterRefactoring >> safeVariableNameFor: aClass temporaries: allTempVars [
	| baseString index newString |
	newString := baseString := 'anObject'.
	index := 0.

	[(allTempVars includes: newString)
		or: [aClass definesInstanceVariable: newString]]
			whileTrue:
				[index := index + 1.
				newString := baseString , index printString].
	^newString
]

{ #category : 'private' }
RBAddParameterRefactoring >> safeVariableNamed: argName for: aClass temporaries: allTempVars [
	| baseString index newString |
	((allTempVars includes: argName)
		or: [aClass definesInstanceVariable: argName]) ifFalse: [ ^ argName ].
	newString := baseString := 'anObject'.
	index := 0.

	[(allTempVars includes: newString)
		or: [aClass definesInstanceVariable: newString]]
			whileTrue:
				[index := index + 1.
				newString := baseString , index printString].
	^newString
]

{ #category : 'private' }
RBAddParameterRefactoring >> senders [

	senders
		ifNil: [ senders := Set new.
			self model allReferencesTo: oldSelector do: [ :each | senders add: each modelClass ]
			].
	^ senders
]

{ #category : 'preconditions' }
RBAddParameterRefactoring >> verifyInitializationExpressionOf: initializer [
	| tree |
	tree := self parserClass
		parseExpression: initializer
		onError: [ :msg :index | self refactoringError: 'Illegal initialization code because:.', msg ].
	tree isValue
		ifFalse: [ self refactoringError: 'The initialization code cannot be a return node or a list of statements' ].
	self checkVariableReferencesIn: tree
]

{ #category : 'preconditions' }
RBAddParameterRefactoring >> violations [

	^ self breakingChangePreconditions flatCollect: [ :each | each violators ]
]
