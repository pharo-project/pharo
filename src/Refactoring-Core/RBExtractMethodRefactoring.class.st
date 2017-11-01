"
I am a refactoring for creating a method from a code fragment.

You can select an interval of some code in a method and call this refactoring to create a new method implementing that code and replace the code by calling this method instead. 
The new method needs to have as many arguments as the number of (temp)variables, the code refers to.

The preconditions are quite complex. The code needs to be parseable valid code. 
"
Class {
	#name : #RBExtractMethodRefactoring,
	#superclass : #RBMethodRefactoring,
	#instVars : [
		'selector',
		'extractionInterval',
		'extractedParseTree',
		'modifiedParseTree',
		'parameters',
		'needsReturn'
	],
	#category : 'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBExtractMethodRefactoring class >> extract: anInterval from: aSelector in: aClass [ 
	^self new
		extract: anInterval
		from: aSelector
		in: aClass
]

{ #category : #'instance creation' }
RBExtractMethodRefactoring class >> model: aRBSmalltalk extract: anInterval from: aSelector in: aClass [ 
	^(self new)
		model: aRBSmalltalk;
		extract: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : #transforming }
RBExtractMethodRefactoring >> checkAssignments: variableNames [ 
	| node outsideVars removeAssigned |
	removeAssigned := variableNames copy.
	node := self placeholderNode.
	outsideVars := variableNames 
				select: [:each | (node whoDefines: each) references: each].
	outsideVars size == 1 
		ifTrue: [self checkSingleAssignment: outsideVars asArray first].
	outsideVars size > 1 
		ifTrue: 
			[self refactoringError: 'Cannot extract assignment without all references.'].
	removeAssigned removeAll: outsideVars.
	(RBReadBeforeWrittenTester readBeforeWritten: removeAssigned
		in: extractedParseTree) isEmpty 
		ifFalse: 
			[self refactoringError: 'Cannot extract assignment if read before written.'].
	removeAssigned 
		do: [:each | (node whoDefines: each) removeTemporaryNamed: each].
	self createTemporariesInExtractedMethodFor: variableNames
]

{ #category : #transforming }
RBExtractMethodRefactoring >> checkReturn [
	needsReturn := self placeholderNode isUsed.
	extractedParseTree containsReturn ifFalse: [^self].
	extractedParseTree lastIsReturn ifTrue: [^self].
	(modifiedParseTree isLast: self placeholderNode) 
		ifFalse: 
			[self refactoringError: 'Couldn''t extract code since it contains a return.'].
	self checkSelfReturns
]

{ #category : #transforming }
RBExtractMethodRefactoring >> checkSelfReturns [
	| searcher |
	searcher := RBParseTreeSearcher new.
	searcher
		matches: '^self' do: [:aNode :answer | answer];
		matches: '^`@anything' do: [:aNode :answer | true].
	(searcher executeTree: extractedParseTree initialAnswer: false) 
		ifTrue: [self placeholderNode asReturn]
]

{ #category : #transforming }
RBExtractMethodRefactoring >> checkSingleAssignment: varName [ 
	((RBReadBeforeWrittenTester isVariable: varName
		readBeforeWrittenIn: extractedParseTree) 
			or: [extractedParseTree containsReturn]) 
			ifTrue: 
				[self 
					refactoringError: 'Cannot extract assignments to temporaries without all references'].
	extractedParseTree 
		addNode: (RBReturnNode value: (RBVariableNode named: varName)).
	modifiedParseTree := RBParseTreeRewriter 
				replace: self methodDelimiter
				with: varName , ' := ' , self methodDelimiter
				in: modifiedParseTree
]

{ #category : #transforming }
RBExtractMethodRefactoring >> checkSpecialExtractions [
	| node |
	node := self placeholderNode parent.
	node isNil ifTrue: [^self].
	(node isAssignment and: [node variable = self placeholderNode]) ifTrue: 
			[self refactoringFailure: 'Cannot extract left hand side of an assignment'].
	node isCascade ifTrue: 
			[self refactoringError: 'Cannot extract first message of a cascaded message']
]

{ #category : #transforming }
RBExtractMethodRefactoring >> checkTemporaries [
	| temps accesses assigned |
	temps := self remainingTemporaries.
	accesses := temps select: [:each | extractedParseTree references: each].
	assigned := accesses select: [:each | extractedParseTree assigns: each].
	assigned isEmpty ifFalse: [self checkAssignments: assigned].
	^parameters := (accesses asOrderedCollection)
				removeAll: assigned;
				yourself
]

{ #category : #transforming }
RBExtractMethodRefactoring >> createTemporariesInExtractedMethodFor: assigned [ 
	assigned do: [:each | extractedParseTree body addTemporaryNamed: each]
]

{ #category : #transforming }
RBExtractMethodRefactoring >> existingSelector [
	"Try to find an existing method instead of creating a new one"

	^class allSelectors detect: [:each | self isMethodEquivalentTo: each]
		ifNone: [nil]
]

{ #category : #initialize-release }
RBExtractMethodRefactoring >> extract: anInterval from: aSelector in: aClass [ 
	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := anInterval
]

{ #category : #transforming }
RBExtractMethodRefactoring >> extractMethod [
	| parseTree isSequence extractCode subtree newCode errorMessage |
	extractCode := self getExtractedSource.
	extractedParseTree := RBParser parseExpression: extractCode
				onError: [:string :pos :parser |  errorMessage := string. parser parseErrorNode: string ].
	extractedParseTree isNil 
		ifTrue: [self refactoringFailure: 'Invalid source to extract'].
		
	extractedParseTree isFaulty
		ifTrue: [self refactoringFailure: 'Invalid source to extract - ', errorMessage.].
		
	(extractedParseTree isSequence 
		and: [extractedParseTree statements isEmpty]) 
			ifTrue: [self refactoringError: 'Select some code to extract'].
	isSequence := extractedParseTree isSequence 
				or: [extractedParseTree isReturn].
	extractedParseTree := RBMethodNode 
				selector: #value
				arguments: #()
				body: (extractedParseTree isSequence 
						ifTrue: [extractedParseTree]
						ifFalse: 
							[RBSequenceNode temporaries: #()
								statements: (OrderedCollection with: extractedParseTree)]).
	extractedParseTree body temporaries isEmpty not 
		ifTrue: [extractedParseTree body temporaries: #()].
	extractedParseTree source: extractCode.
	parseTree := class parseTreeFor: selector.
	parseTree isNil 
		ifTrue: [self refactoringFailure: 'Could not parse ' , selector printString].
	subtree := isSequence 
				ifTrue: 
					[RBParseTreeSearcher 
						treeMatchingStatements: extractedParseTree body formattedCode
						in: parseTree]
				ifFalse: [RBParseTreeSearcher treeMatching: extractCode in: parseTree].
	subtree isNil 
		ifTrue: [self refactoringFailure: 'Could not extract code from method'].
	newCode := self methodDelimiter.
	isSequence 
		ifTrue: 
			[| stmts |
			stmts := extractedParseTree body statements.
			stmts isEmpty 
				ifFalse: 
					[stmts last isAssignment 
						ifTrue: 
							[| name |
							name := stmts last variable name.
							(self shouldExtractAssignmentTo: name) 
								ifFalse: 
									[newCode := '<1s> := <2s>' expandMacrosWith: name with: newCode.
									stmts at: stmts size put: stmts last value]]]].
	modifiedParseTree := isSequence 
				ifTrue: 
					[RBParseTreeRewriter 
						replaceStatements: subtree formattedCode
						with: newCode
						in: parseTree
						onInterval: extractionInterval]
				ifFalse: 
					[RBParseTreeRewriter 
						replace: subtree formattedCode
						with: newCode
						in: parseTree
						onInterval: extractionInterval]
]

{ #category : #transforming }
RBExtractMethodRefactoring >> getExtractedSource [
	| source |
	source := class sourceCodeFor: selector.
	((extractionInterval first between: 1 and: source size) 
		and: [extractionInterval last between: 1 and: source size]) 
			ifFalse: [self refactoringFailure: 'Invalid interval'].
	^source copyFrom: extractionInterval first to: extractionInterval last
]

{ #category : #transforming }
RBExtractMethodRefactoring >> getNewMethodName [
	| newSelector methodName newMethodName |
	methodName := RBMethodName new.
	methodName arguments: parameters.
	
	[newMethodName := self requestMethodNameFor: methodName.
	newMethodName isNil 
		ifTrue: [self refactoringFailure: 'Did not extract code'].
	newSelector := newMethodName selector.
	(self checkMethodName: newSelector in: class) 
		ifFalse: 
			[self refactoringWarning: newSelector , ' is not a valid selector name.'.
			newSelector := nil].
	(class hierarchyDefinesMethod: newSelector asSymbol) 
		ifTrue: 
			[(self shouldOverride: newSelector in: class) ifFalse: [newSelector := nil]].
	newSelector isNil] 
			whileTrue: [].
	parameters := newMethodName arguments asOrderedCollection.
	^newSelector asSymbol
]

{ #category : #transforming }
RBExtractMethodRefactoring >> isMethodEquivalentTo: aSelector [ 
	selector == aSelector ifTrue: [^false].
	aSelector numArgs ~~ parameters size ifTrue: [^false].
	(self isParseTreeEquivalentTo: aSelector) ifFalse: [^false].
	self reorderParametersToMatch: aSelector.
	^true
]

{ #category : #transforming }
RBExtractMethodRefactoring >> isParseTreeEquivalentTo: aSelector [ 
	| tree definingClass |
	definingClass := class whoDefinesMethod: aSelector.
	tree := definingClass parseTreeFor: aSelector.
	tree isNil ifTrue: [^false].
	tree isPrimitive ifTrue: [^false].
	(tree body equalTo: extractedParseTree body
		exceptForVariables: (tree arguments collect: [:each | each name])) 
			ifFalse: [^false].
	(definingClass = class or: 
			[(tree superMessages detect: 
					[:each | 
					(class superclass whichClassIncludesSelector: aSelector) 
						~= (definingClass superclass whichClassIncludesSelector: each)]
				ifNone: [nil]) isNil]) 
		ifFalse: [^false].
	^self shouldUseExistingMethod: aSelector
]

{ #category : #transforming }
RBExtractMethodRefactoring >> methodDelimiter [
	^'#''place.holder.for.method'''
]

{ #category : #transforming }
RBExtractMethodRefactoring >> nameNewMethod: aSymbol [
	| args newSend |
	args := parameters collect: [ :parm | RBVariableNode named: parm ].
	extractedParseTree renameSelector: aSymbol andArguments: args asArray.
	aSymbol numArgs = 0
		ifTrue: [ modifiedParseTree := RBParseTreeRewriter replace: self methodDelimiter with: 'self ' , aSymbol asString in: modifiedParseTree.
			^ self ].
	newSend := String
		streamContents: [ :str | 
			str nextPutAll: 'self '.
			aSymbol keywords
				with: parameters
				do: [ :key :arg | 
					str
						nextPutAll: key asString;
						nextPut: $ ;
						nextPutAll: arg asString;
						nextPut: $  ] ].
	modifiedParseTree := RBParseTreeRewriter replace: self methodDelimiter with: newSend in: modifiedParseTree
]

{ #category : #transforming }
RBExtractMethodRefactoring >> placeholderNode [
	| node |
	node := RBParseTreeSearcher treeMatching: self methodDelimiter
				in: modifiedParseTree.
	node isNil ifTrue: [self refactoringFailure: 'Cannot extract code'].
	^node
]

{ #category : #preconditions }
RBExtractMethodRefactoring >> preconditions [
	^(RBCondition definesSelector: selector in: class) 
		& (RBCondition withBlock: 
					[self extractMethod.
					self checkSpecialExtractions.
					self checkReturn.
					needsReturn ifTrue: [extractedParseTree addReturn].
					self checkTemporaries.
					true])
]

{ #category : #transforming }
RBExtractMethodRefactoring >> remainingTemporaries [
	| temps |
	temps := modifiedParseTree allDefinedVariables asSet.
	extractedParseTree allDefinedVariables 
		do: [:each | temps remove: each ifAbsent: []].
	^temps
]

{ #category : #transforming }
RBExtractMethodRefactoring >> reorderParametersToMatch: aSelector [ 
	| tree dictionary |
	tree := class parseTreeFor: aSelector.
	dictionary := Dictionary new.
	tree body equalTo: extractedParseTree body withMapping: dictionary.
	parameters := tree arguments collect: 
					[:each | 
					dictionary at: each name
						ifAbsent: 
							[self 
								refactoringFailure: 'An internal error occured, please report this error.']]
]

{ #category : #printing }
RBExtractMethodRefactoring >> storeOn: aStream [ 
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' extract: '.
	extractionInterval storeOn: aStream.
	aStream
		nextPutAll: ' from: #';
		nextPutAll: selector;
		nextPutAll: ' in: '.
	class storeOn: aStream.
	aStream nextPut: $)
]

{ #category : #transforming }
RBExtractMethodRefactoring >> transform [
	| existingSelector |
	existingSelector := self existingSelector.
	self nameNewMethod: (existingSelector isNil 
				ifTrue: [self getNewMethodName]
				ifFalse: [existingSelector]).
	existingSelector isNil 
		ifTrue: 
			[class compile: extractedParseTree newSource
				withAttributesFrom: (class methodFor: selector)].
	class compileTree: modifiedParseTree
]
