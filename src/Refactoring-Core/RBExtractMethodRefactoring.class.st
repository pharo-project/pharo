"
I am a refactoring for creating a method from a code fragment.

You can select an interval of some code in a method and call this refactoring to create a new method implementing that code and replace the code by calling this method instead. 
The new method needs to have as many arguments as the number of (temp)variables, the code refers to.

The preconditions are quite complex. The code needs to be parseable valid code. 
"
Class {
	#name : 'RBExtractMethodRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'extractionInterval',
		'extractedParseTree',
		'modifiedParseTree',
		'parameters',
		'needsReturn',
		'parameterMap',
		'newExtractedSelector'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'instance creation' }
RBExtractMethodRefactoring class >> extract: anInterval from: aSelector in: aClass [
	^ self new
		extract: anInterval
		from: aSelector
		in: aClass
]

{ #category : 'instance creation' }
RBExtractMethodRefactoring class >> extractSource: aString from: aSelector in: aClass [ 

	^ self new
		extractSource: aString 
		from: aSelector
		in: aClass
]

{ #category : 'instance creation' }
RBExtractMethodRefactoring class >> model: aRBSmalltalk extract: anInterval from: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		extract: anInterval
			from: aSelector
			in: aClass;
		yourself
]

{ #category : 'instance creation' }
RBExtractMethodRefactoring class >> model: aRBSmalltalk extractSource: aString from: aSelector in: aClass [
	^ self new
		model: aRBSmalltalk;
		extractSource: aString
		from: aSelector
		in: aClass;
		yourself
]

{ #category : 'preconditions' }
RBExtractMethodRefactoring >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: selector in: class).
		  (RBCondition withBlock: [
			   self checkSpecialExtractions.
			   self checkReturn.
			   true ]) }
]

{ #category : 'transforming' }
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
	(OCReadBeforeWrittenTester readBeforeWritten: removeAssigned
		in: extractedParseTree) ifNotEmpty:
			[self refactoringError: 'Cannot extract assignment if read before written.'].
	removeAssigned do: [:each | (node whoDefines: each) removeTemporaryNamed: each].
	self updateTemporariesInExtractedMethodFor: variableNames
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> checkReturn [

	needsReturn := self placeholderNode isUsedAsReturnValue.
	extractedParseTree containsReturn ifFalse: [^self].
	extractedParseTree lastIsReturn ifTrue: [^self].
	"Since we have a return that is not last in the extracted tree,
	extraction is only possible if the modified method's last statement is being extracted"
	(modifiedParseTree isLast: self placeholderNode)
		ifFalse:
			[self refactoringError: 'Couldn''t extract code since it contains a return.'].
	self checkSelfReturns
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> checkSelfReturns [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: '^self' do: [ :aNode :answer | answer ];
		matches: '^`@anything' do: [ :aNode :answer | true ].
	(searcher executeTree: extractedParseTree initialAnswer: false)
		ifTrue: [ self placeholderNode asReturn ]
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> checkSingleAssignment: varName [
	((OCReadBeforeWrittenTester isVariable: varName
		readBeforeWrittenIn: extractedParseTree)
			or: [extractedParseTree containsReturn])
			ifTrue:
				[self
					refactoringError: 'Cannot extract assignments to temporaries without all references'].
	extractedParseTree
		addNode: (OCReturnNode value: (OCVariableNode named: varName)).
	modifiedParseTree := self parseTreeRewriterClass
				replace: self methodDelimiter
				with: varName , ' := ' , self methodDelimiter
				in: modifiedParseTree
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> checkSpecialExtractions [
	| node |
	node := self placeholderNode parent.
	node ifNil: [^self].
	(node isAssignment and: [node variable = self placeholderNode]) ifTrue:
			[self refactoringError: 'Cannot extract left hand side of an assignment'].
	node isCascade ifTrue:
			[self refactoringError: 'Cannot extract first message of a cascaded message']
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> checkTemporaries [
	| temps accesses assigned |
	temps := self remainingTemporaries.
	accesses := temps select: [:each | extractedParseTree references: each].
	assigned := accesses select: [:each | extractedParseTree assigns: each].
	assigned ifNotEmpty: [self checkAssignments: assigned].
	^parameters := (accesses asOrderedCollection)
				removeAll: assigned;
				yourself
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> compileExtractedMethod [

	| cls |
	self renameAllParameters.
	cls := self classObjectFor: self requestExtractionClass.
	^ cls
		  compile: extractedParseTree newSource
		  withAttributesFrom: (class methodFor: selector)
]

{ #category : 'preparation' }
RBExtractMethodRefactoring >> createModifiedParseTreeForNonSequenceSelection [

	| parseTree subtree newCode |
	parseTree := class parseTreeForSelector: selector.
	parseTree ifNil: [ self refactoringError: 'Could not parse ' , selector printString ].
	subtree := self parseTreeSearcherClass treeMatching: extractedParseTree sourceCode in: parseTree.
	subtree ifNil: [ self refactoringError: 'Could not extract code from method' ].
	newCode := self methodDelimiter.
	modifiedParseTree := self parseTreeRewriterClass
		replace: subtree formattedCode
		with: newCode
		in: parseTree
		onInterval: extractionInterval
]

{ #category : 'preparation' }
RBExtractMethodRefactoring >> createModifiedParseTreeForSequenceSelection [

	| parseTree subtree stmts newCode |
	parseTree := class parseTreeForSelector: selector.
	parseTree
		ifNil: [ self refactoringError: 'Could not parse ' , selector printString ].
	subtree := self parseTreeSearcherClass
		treeMatchingStatements: extractedParseTree body formattedCode
		in: parseTree.
	subtree ifNil: [ self refactoringError: 'Could not extract code from method' ].
	newCode := self methodDelimiter.
	stmts := extractedParseTree body statements.
	stmts
		ifNotEmpty: [ stmts last isAssignment
			ifTrue: [ | name |
					name := stmts last variable name.
					(self shouldExtractAssignmentTo: name)
						ifFalse: [ newCode := '<1s> := <2s>' expandMacrosWith: name with: newCode.
							stmts at: stmts size put: stmts last value ] ] ].
	modifiedParseTree := self parseTreeRewriterClass
		replaceStatements: subtree formattedCode
		with: newCode
		in: parseTree
		onInterval: extractionInterval
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> existingSelector [
	"Try to find an existing method instead of creating a new one"

	^ self requestExistingSelector
		ifNil: [ self selectorsToSearch
				detect: [ :each | self isMethodEquivalentTo: each ]
				ifNone: [ nil ] ]
]

{ #category : 'initialization' }
RBExtractMethodRefactoring >> extract: anInterval from: aSelector in: aClass [
	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := anInterval
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> extractMethod [
	| isSequence extractCode |
	extractCode := self getExtractedSource.
	self extractParseTreeForSelection: extractCode.
	(extractedParseTree isSequence
		and: [ extractedParseTree statements isEmpty ])
		ifTrue: [ self refactoringError: 'Select some code to extract' ].
	isSequence := extractedParseTree isSequence
		or: [ extractedParseTree isReturn ].
		
	self wrapExtractedParseTreeInMethodNode: extractCode.

	isSequence
		ifTrue: [ self createModifiedParseTreeForSequenceSelection ]
		ifFalse: [ self createModifiedParseTreeForNonSequenceSelection ].
]

{ #category : 'preparation' }
RBExtractMethodRefactoring >> extractParseTreeForSelection: extractCode [

	extractedParseTree := self parserClass new
		source: extractCode;
		parseExpression.
	extractedParseTree
		ifNil: [ self refactoringError: 'Invalid source to extract' ].
	extractedParseTree isFaulty
		ifTrue: [ self
			refactoringError: 'Invalid source to extract - ' , extractedParseTree allErrorNotices first messageText ].
]

{ #category : 'instance creation' }
RBExtractMethodRefactoring >> extractSource: aString from: aSelector in: aClass [ 

	class := self classObjectFor: aClass.
	selector := aSelector.
	extractionInterval := self getIntervalForSource: aString
]

{ #category : 'accessing' }
RBExtractMethodRefactoring >> extractedParseTree [
	^ extractedParseTree
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> getExtractedSource [
	| source |
	source := class sourceCodeFor: selector.
	source ifNil: [ self refactoringError: 'Invalid source' ].
	((extractionInterval first between: 1 and: (self startLimit: source size))
		and: [extractionInterval last between: 1 and: source size])
			ifFalse: [self refactoringError: 'Invalid interval'].
	^source copyFrom: extractionInterval first to: extractionInterval last
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> getIntervalForSource: aString [
	"I return interval where aString is situated in the `selector`"

	| method expression searcher matchedNode |
	method := class methodFor: selector.
	expression := OCParser parseExpression: aString.

	searcher := OCParseTreeSearcher new.
	searcher matchesTree: expression do: [ :aNode :answer | aNode ].
	matchedNode := searcher executeTree: method ast.
	matchedNode ifNil: [ self refactoringError: 'Could not find given source in the method.' ].
	^ matchedNode start to: matchedNode stop
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> getNewMethodName [
	| newSelector methodName newMethodName |
	methodName := RBMethodName new.
	methodName arguments: parameters.

	[newMethodName := self requestMethodNameFor: methodName.
	newMethodName ifNil: [self refactoringError: 'Did not extract code'].
	newSelector := newMethodName selector.
	(self checkMethodName: newSelector in: class)
		ifFalse:
			[self refactoringWarning: newSelector , ' is not a valid selector name.'.
			newSelector := nil].
	(class hierarchyDefinesMethod: newSelector asSymbol)
		ifTrue:
			[(self shouldOverride: newSelector in: class) ifFalse: [ newSelector := nil ]].
	newSelector isNil]
			whileTrue: [].
	parameters := newMethodName arguments asOrderedCollection.
	newMethodName renameMap ifNotEmpty: [parameterMap := newMethodName renameMap].
	^newSelector asSymbol
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> isMethodEquivalentTo: aSelector [

	selector == aSelector ifTrue: [^false].

	aSelector numArgs ~~ parameters size ifTrue: [^false].

	(self isParseTreeEquivalentTo: aSelector) ifFalse: [^false].
	self reorderParametersToMatch: aSelector.
	^true
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> isParseTreeEquivalentTo: aSelector [

	| tree definingClass |
	definingClass := class whichClassIncludesSelector: aSelector.
	tree := definingClass parseTreeForSelector: aSelector.
	tree ifNil: [ ^ false ].
	tree isPrimitive ifTrue: [ ^ false ].
	needsReturn ifFalse: [ tree := self removeReturnsOf: tree ].
	(tree body
		 equalTo: extractedParseTree body
		 exceptForVariables: (tree arguments collect: [ :each | each name ]))
		ifFalse: [ ^ false ].
	(definingClass = class or: [
		 (tree superMessages anySatisfy: [ :each |
			  (class superclass whichClassIncludesSelector: aSelector)
			  ~= (definingClass superclass whichClassIncludesSelector: each) ])
			 not ]) ifFalse: [ ^ false ].
	^ true
]

{ #category : 'accessing' }
RBExtractMethodRefactoring >> method [
	^ class realClass >> selector
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> methodDelimiter [
	^'#''place.holder.for.method'''
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> nameNewMethod: aSymbol [
	| args newSend |
	newExtractedSelector := aSymbol .
	args := parameters collect: [ :parm | OCVariableNode named: parm ].
	extractedParseTree renameSelector: aSymbol andArguments: args asArray.
	aSymbol numArgs = 0
		ifTrue: [ modifiedParseTree := OCParseTreeRewriter replace: self methodDelimiter with: 'self ' , aSymbol asString in: modifiedParseTree.
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
	modifiedParseTree := OCParseTreeRewriter replace: self methodDelimiter with: newSend in: modifiedParseTree.
	self parameterMap do: [ :e |
	(self parseTreeRewriterClass rename: e newName to: e name) executeTree: modifiedParseTree ]
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> newExtractedSelector [
	^ newExtractedSelector
]

{ #category : 'accessing' }
RBExtractMethodRefactoring >> parameterMap [
	^ parameterMap ifNil: [ parameterMap := { } ]
]

{ #category : 'accessing' }
RBExtractMethodRefactoring >> parameterMap: aDictionary [
	parameterMap := aDictionary
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> parameters [

	^ parameters
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> parameters: anOrderedCollection [
	parameters := anOrderedCollection
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> placeholderNode [
	| node |
	node := self parseTreeSearcherClass
		treeMatching: self methodDelimiter
		in: modifiedParseTree.
	node ifNil: [ self refactoringError: 'Cannot extract code' ].
	^ node
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> prepareForExecution [ 

	self extractMethod.
	self checkTemporaries.
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> privateTransform [

	| existingSelector |
	needsReturn ifTrue: [ extractedParseTree addReturn ].
	existingSelector := self existingSelector.
	self nameNewMethod: (existingSelector
			 ifNil: [ newExtractedSelector ifNil: [ self getNewMethodName ] ]
			 ifNotNil: [ existingSelector ]).
	existingSelector ifNil: [ self compileExtractedMethod ].
	class compileTree: modifiedParseTree
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> remainingTemporaries [
	| temps |
	temps := modifiedParseTree allDefinedVariables asSet.
	extractedParseTree allDefinedVariables
		do: [:each | temps remove: each ifAbsent: []].
	^temps
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> renameAllParameters [
	self parameterMap do: [ :each |
			self renameParameterWith: each name to: each newName ]
]

{ #category : 'renaming' }
RBExtractMethodRefactoring >> renameNode: aParseTree withOldName: oldName toWithName: newName [
	(self parseTreeRewriterClass rename: oldName to: newName) executeTree: aParseTree
]

{ #category : 'renaming' }
RBExtractMethodRefactoring >> renameParameterWith: oldName to: newName [
	self renameNode: extractedParseTree withOldName: oldName toWithName: newName
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> reorderParametersToMatch: aSelector [
	| tree dictionary |
	tree := class parseTreeForSelector: aSelector.
	needsReturn ifFalse: [ tree := self removeReturnsOf: tree ].
	dictionary := Dictionary new.
	tree body equalTo: extractedParseTree body withMapping: dictionary.
	parameters := tree arguments collect:
					[:each |
					dictionary at: each name
						ifAbsent:
							[self
								refactoringError: 'An internal error occured, please report this error.']]
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> requestExistingSelector [
	^ [(self options at: #existingSelector) value: self]
			on: Exception
			do: [ :e | nil ]
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> requestExtractionClass [
	^ (self options at: #extractionClass ifAbsent: [ [:ref | class ]]) value: self
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> selectorsToSearch [
	^ class allSelectors
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> startLimit: aNumber [
	^ aNumber
]

{ #category : 'storing' }
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

{ #category : 'accessing' }
RBExtractMethodRefactoring >> targetClass [
	^ class
]

{ #category : 'transforming' }
RBExtractMethodRefactoring >> updateTemporariesInExtractedMethodFor: assigned [
	"Add temporaries in extract method"
	assigned do: [:each | extractedParseTree body addTemporaryNamed: each]
]

{ #category : 'renaming' }
RBExtractMethodRefactoring >> validateRenameNode: aParseTree withOldName: oldName toWithName: newName [
	|conditions block|
	conditions := ((RBCondition isValidInstanceVariableName: newName for: class)
		& (RBCondition definesSelector: selector in: class)
		& (RBCondition definesInstanceVariable: newName in: class) not
		& (RBCondition definesClassVariable: newName in: class) not).
	conditions check
		ifFalse: [ block := conditions errorBlock.
			block
				ifNotNil: [ self refactoringWarning: conditions errorString with: block ]
				ifNil: [ self refactoringError: conditions errorString ] ].
	(self parameterMap values includes: newName) ifTrue: [
		self refactoringError: newName asString , ' is already defined as parameter' ].
	(aParseTree whoDefines: newName)
		ifNotNil: [ self refactoringError: newName asString , ' is already defined' ].
	(aParseTree allDefinedVariables includes: newName)
		ifTrue: [ self refactoringError: newName asString , ' is already defined' ]
]

{ #category : 'renaming' }
RBExtractMethodRefactoring >> validateRenameOf: oldName to: newName [
	self validateRenameNode: extractedParseTree withOldName: oldName toWithName: newName
]

{ #category : 'preparation' }
RBExtractMethodRefactoring >> wrapExtractedParseTreeInMethodNode: extractCode [

	extractedParseTree := OCMethodNode
		selector: #value
		arguments: #()
		body:
			(extractedParseTree isSequence
				ifTrue: [ extractedParseTree ]
				ifFalse: [ OCSequenceNode
						temporaries: #()
						statements: (OrderedCollection with: extractedParseTree) ]).
	extractedParseTree body temporaries
		ifNotEmpty: [ extractedParseTree body temporaries: #() ].
	extractedParseTree source: extractCode.
]
