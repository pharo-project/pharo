"
A Breakpoint is an object used for stopping the execution of the program in a determined instruction, normally with debugging purposes. 
There are three types of breakpoints:
-halt once: These are triggered once, and then removed.
-halt always: Once installed, they have to be explicitly removed (by using #remove on the instance side, or #removeAll on class side)
-halt on condition: Before being installed prompts the user for a condition (a block with a boolean expression), and it is only triggered when that condition is met.

Breakpoints are installed via SmartSuggestions menu (available for ASTMethodNode, ASTMessageNode and ASTAssignmentNode), or programmatically:

""(Breakpoint inMethod: (Bar>>#foo2) inNode: ((Bar>>#foo2) ast allChildren at: 9) )
break: #when: withArguments: { [ RFCounter counter isZero ] }
""


"
Class {
	#name : 'Breakpoint',
	#superclass : 'Object',
	#instVars : [
		'#condition',
		'#link',
		'#node',
		'#level',
		'#options',
		'#targetInstance => WeakSlot',
		'#once',
		'#enabled',
		'#group',
		'#onCount',
		'#count',
		'#breakOnCount'
	],
	#classVars : [
		'#AllBreakpoints'
	],
	#category : 'Reflectivity-Breakpoints',
	#package : 'Reflectivity',
	#tag : 'Breakpoints'
}

{ #category : 'accessing' }
Breakpoint class >> addBreakpoint: aBreakpoint [

	self all add: aBreakpoint.
	self notifyBreakpointAdded: aBreakpoint
]

{ #category : 'accessing' }
Breakpoint class >> all [

	^ AllBreakpoints ifNil: [ AllBreakpoints := OrderedCollection new ]
]

{ #category : 'API - object-centric' }
Breakpoint class >> breakOn: aSelector inObject: anObject [

	| ast |
	ast := (anObject class lookupSelector: aSelector) ast.
	^ self breakOnAST: ast inObject: anObject
]

{ #category : 'API - object-centric' }
Breakpoint class >> breakOnAST: aNode inObject: anObject [

	| breakpoint |
	breakpoint := self new.
	breakpoint node: aNode.
	breakpoint scopeTo: anObject.
	breakpoint install.
	^ breakpoint
]

{ #category : 'API - object-centric' }
Breakpoint class >> breakOnceOn: aSelector inObject: anObject [

	| ast |
	ast := (anObject class lookupSelector: aSelector) ast.
	^ self breakOnceOnAST: ast inObject: anObject
]

{ #category : 'API - object-centric' }
Breakpoint class >> breakOnceOnAST: aNode inObject: anObject [

	| breakpoint |
	breakpoint := self new.
	breakpoint once: true.
	breakpoint node: aNode.
	breakpoint scopeTo: anObject.
	breakpoint install.
	^ breakpoint
]

{ #category : 'accessing' }
Breakpoint class >> browseAll [
	<script>

	| methods |
	methods := self all flatCollect: [ :breakpoint | breakpoint link methods ].
	(Smalltalk tools toolNamed: #messageList) browse: methods
]

{ #category : 'testing' }
Breakpoint class >> checkBreakConditionValue: aValue [

	(aValue isKindOf: Boolean) ifTrue: [ ^ aValue ].
	Error signal: 'Breakpoint condition returned a non boolean value'
]

{ #category : 'cleanup' }
Breakpoint class >> cleanUp [

	self removeAll
]

{ #category : 'system announcements' }
Breakpoint class >> handleClassRemoved: anAnnouncement [

	self all copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m methodClass = anAnnouncement classRemoved ]
			ifFound: [ breakpoint removeFromClass: anAnnouncement classRemoved ] ]
]

{ #category : 'system announcements' }
Breakpoint class >> handleMethodModified: anAnnouncement [
	self removeFromMethod: anAnnouncement oldMethod
]

{ #category : 'system announcements' }
Breakpoint class >> handleMethodRemoved: anAnnouncement [
	self removeFromMethod: anAnnouncement method
]

{ #category : 'class initialization' }
Breakpoint class >> initialize [
	self registerInterestToSystemAnnouncement
]

{ #category : 'querying' }
Breakpoint class >> isInstalledIn: aMethod [

	self all do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ ^ true ]
	].

	^ false
]

{ #category : 'notifications' }
Breakpoint class >> notifyBreakpointAdded: aBreakpoint [

	self codeSupportAnnouncer announce: (BreakpointAdded on: aBreakpoint nodes: aBreakpoint link nodes)
]

{ #category : 'notifications' }
Breakpoint class >> notifyBreakpointHit: aBreakpoint inContext: aContext node: node [

	self codeSupportAnnouncer announce: (BreakpointHit on: aBreakpoint nodes: { node })
]

{ #category : 'notifications' }
Breakpoint class >> notifyBreakpointRemoved: aBreakpoint fromNodes: nodes [

	self codeSupportAnnouncer announce: (BreakpointRemoved on: aBreakpoint nodes: nodes)
]

{ #category : 'class initialization' }
Breakpoint class >> registerInterestToSystemAnnouncement [

	<systemEventRegistration>
	self codeChangeAnnouncer unsubscribe: self.
	self codeChangeAnnouncer weak
		when: MethodRemoved send: #handleMethodRemoved: to: self;
		when: MethodModified send: #handleMethodModified: to: self;
		when: ClassRemoved send: #handleClassRemoved: to: self
]

{ #category : 'cleanup' }
Breakpoint class >> removeAll [
	<script>

	self all copy do: #remove
]

{ #category : 'accessing' }
Breakpoint class >> removeBreakpoint: aBreakpoint [

	| nodes |
	nodes := aBreakpoint link nodes copy.
	self all remove: aBreakpoint.
	self notifyBreakpointRemoved: aBreakpoint fromNodes: nodes
]

{ #category : 'cleanup' }
Breakpoint class >> removeFrom: aNode [

	aNode breakpoints do: [ :breakpoint | breakpoint remove ]
]

{ #category : 'cleanup' }
Breakpoint class >> removeFromMethod: aMethod [

	self all copy do: [ :breakpoint |
		breakpoint link methods
			detect: [ :m | m == aMethod ]
			ifFound: [ breakpoint removeFromMethod: aMethod ] ]
]

{ #category : 'api' }
Breakpoint >> addToGroup: aBreakpointGroup [
	"puts the breakpoint in a new group and sets the group variable to that group"
	group removeBreakpoint: self.
	aBreakpointGroup addBreakpoint: self.
	^ group := aBreakpointGroup
]

{ #category : 'api' }
Breakpoint >> breakInContext: aContext node: aNode [
	 <debuggerCompleteToSender>

	 self class notifyBreakpointHit: self inContext: aContext node: aNode.
	 self isEnabled ifFalse: [ ^ self ].
	 self onCount ifTrue: [ (self increaseCount = self breakOnCount) ifFalse: [^ self]].
	 self once ifTrue: [ self disable ].
	 Break break
]

{ #category : 'links' }
Breakpoint >> breakLink [
	"for now it should just halt in base level"

	^ MetaLink new
		  metaObject: self;
		  selector: #breakInContext:node:;
		  options: options;
		  arguments: #(#context #node)
]

{ #category : 'links' }
Breakpoint >> breakLinkConditional [

	^self breakLink condition: condition arguments: #(context)
]

{ #category : 'api' }
Breakpoint >> breakOnCount [

	^breakOnCount ifNil:[ breakOnCount := 0]
]

{ #category : 'api' }
Breakpoint >> breakOnCount: anInterger [

	breakOnCount := anInterger
]

{ #category : 'TODO' }
Breakpoint >> condition [
	"this method doesnt really work well yet because the condition is not just the condition text but a collection"
	^ condition
]

{ #category : 'api' }
Breakpoint >> condition: aCondition [

	condition := aCondition.
	self link: self breakLinkConditional
]

{ #category : 'api' }
Breakpoint >> conditionBlockProducerMethodAST: conditionString [
	| conditionBlockProducerMethodAST conditionAST |
	conditionAST := self parserClass parseExpression: conditionString.
	conditionBlockProducerMethodAST := OCMethodNode
		selector: #conditionBlockGenerator
		body:
			(OCReturnNode
				value:
					(OCBlockNode
						arguments: {(OCVariableNode named: 'ThisContext')}
						body:
							(OCMessageNode
								receiver: (OCVariableNode named: 'Breakpoint')
								selector: #checkBreakConditionValue:
								arguments: {conditionAST}) asSequenceNode)) asSequenceNode.
	conditionBlockProducerMethodAST := self
		rewriteASTToSimulateExecutionInADifferentContext: conditionBlockProducerMethodAST.
	conditionBlockProducerMethodAST
		source: conditionBlockProducerMethodAST formattedCode.
	"The following commented code is a failed attempt at adding a comment to the conditionBlockProducerMethod source code, saying that it is autogenerated and that modifying it is pointless."
	"conditionBlockProducerMethodAST comments: {ASTCommentNode with: 'Auto generated' at: (conditionBlockProducerMethodAST selector size)}."
	"Setting a source code for the AST, so that inspecting it, for example after an exception occured, works."
	^ conditionBlockProducerMethodAST
]

{ #category : 'accessing' }
Breakpoint >> count [
	^count ifNil: [ count := 0 ]
]

{ #category : 'accessing' }
Breakpoint >> count: anInteger [

	count := anInteger
]

{ #category : 'api' }
Breakpoint >> createCondition [

	self condition: self requestConditionBlock
]

{ #category : 'api' }
Breakpoint >> disable [
	self enabled: false
]

{ #category : 'api' }
Breakpoint >> enable [
	self enabled: true
]

{ #category : 'api' }
Breakpoint >> enabled: aBoolean [
	enabled := aBoolean
]

{ #category : 'api' }
Breakpoint >> generateConditionBlockFrom: aString [
	| conditionBlockProducerMethodAST |
	conditionBlockProducerMethodAST := self conditionBlockProducerMethodAST: aString.

	^ conditionBlockProducerMethodAST generateMethod valueWithReceiver: nil
]

{ #category : 'accessing' }
Breakpoint >> group [
	"returns the group of the breakpoint"
	^ group
]

{ #category : 'private' }
Breakpoint >> increaseCount [

	^self count: (self count + 1)
]

{ #category : 'initialization' }
Breakpoint >> initialize [

	self class initializeSlots: self.
	options := #(+ optionCompileOnLinkInstallation).
	BreakpointGroup newBreakpoint: self.
	group := BreakpointGroup ungroupedBreakpointsGroup
]

{ #category : 'install' }
Breakpoint >> install [

	self setAsNodeProperty.
	self isObjectCentric
		ifTrue: [ self targetInstance link: self link toAST: self node.
			self class addBreakpoint: self.
			^ self ].
	self node link: self link.
	self class addBreakpoint: self
]

{ #category : 'testing' }
Breakpoint >> isEnabled [

	^ enabled ifNil: [ enabled := true ]
]

{ #category : 'testing' }
Breakpoint >> isObjectCentric [

	^ self targetInstance isNotNil
]

{ #category : 'testing' }
Breakpoint >> isVariableBreakpoint [

	^ false
]

{ #category : 'accessing' }
Breakpoint >> level [

	^ level
]

{ #category : 'accessing' }
Breakpoint >> level: aLevel [

	level := aLevel
]

{ #category : 'accessing' }
Breakpoint >> link [

	^ link ifNil: [ link := self breakLink ]
]

{ #category : 'accessing' }
Breakpoint >> link: aMetaLink [

	link := aMetaLink
]

{ #category : 'accessing' }
Breakpoint >> node [

	^ node
]

{ #category : 'accessing' }
Breakpoint >> node: aNode [

	node := aNode
]

{ #category : 'api' }
Breakpoint >> onCount [

	^ onCount ifNil: [ onCount := false ]
]

{ #category : 'api' }
Breakpoint >> onCount: aBoolean [

	^ onCount := aBoolean
]

{ #category : 'api' }
Breakpoint >> once [

	^ once ifNil: [ once := false ]
]

{ #category : 'api' }
Breakpoint >> once: aBoolean [

	once := aBoolean
]

{ #category : 'accessing' }
Breakpoint >> options [

	^ options
]

{ #category : 'accessing' }
Breakpoint >> options: anArray [

	options := anArray
]

{ #category : 'api' }
Breakpoint >> parserClass [
	^ OCParser
]

{ #category : 'removing' }
Breakpoint >> remove [

	self removeFromNodeProperty.
	self removeFromBreakpointGroups.
	self class removeBreakpoint: self.
	link uninstall
]

{ #category : 'removing' }
Breakpoint >> removeFromBreakpointGroups [
	"just in case the breakpoint is not removed correctly this will remove the reference to the group"
 	group := nil.

]

{ #category : 'removing' }
Breakpoint >> removeFromClass: aClass [
	"we just remove this breakpoint from the cache, overridden by VariableBreakpoint to do more"
	self class removeBreakpoint: self
]

{ #category : 'removing' }
Breakpoint >> removeFromMethod: aMethod [
	"we just remove this breakpoint from the cache, overridden by VariableBreakpoint to do more"
	self class removeBreakpoint: self
]

{ #category : 'removing' }
Breakpoint >> removeFromNodeProperty [

	self node removeBreakpoint: self
]

{ #category : 'api' }
Breakpoint >> requestConditionBlock [
	| script |
	script := 'true'.
	script := MorphicUIManager new
		request: 'This expression will be evaluated in the context of the position of this breakpoint.
		You can use variables, globals, `thisContext`, `self` and `super`.
		`:=` and Pool variables are not supported`'
		initialAnswer: script
		title: 'Break on what condition?'.
	script isEmptyOrNil ifTrue: [ CmdCommandAborted signal ].
	^ self generateConditionBlockFrom: script
]

{ #category : 'api' }
Breakpoint >> rewriteASTToSimulateExecutionInADifferentContext: anAST [
	| semanticallyAnalysedMethodAST allUndeclaredVariableNodes allUndeclaredVariableSymbols rewriter rewrittenConditionBlockProducerAST allMessageNodes receiverSuperclassValueNode intermediateAST analysedIntermediateAST rewriter2 |
	"AnAST is the AST of a method returning a block taking a context as its argument named `ThisContext` and whose body is the condition of this conditional breakpoint.
	The goal is to rewrite this AST so that evaluating the block by passing it a context as argument will evaluate its body as it would have been evaluated in the passed context.
	To do this, we rewrite anAST following these three rules:
	1) Rewriting references to variables named 'ThisContext' into context lookups to avoid conflicts with the 'ThisContext' argument of the block.
	For example, a reference to a variable named ThisContext is rewritten into `ThisContext readVariableNamed: #ThisContext`
	2) Replacing all message nodes whose receiver is super with alternative ast replicating the method lookup a message send to super has.
		For example, 'super foo: 1' is rewritten into 'ThisContext receiver perform: #foo withArguments: 1 inSuperclass: ThisContext receiver class superclass'
	3) Rewriting references to undeclared variables into context lookup
		For example, a reference to a variable named flower is rewritten into `ThisContext readVariableNamed: #flower`
	4) Rewriting references to `thisContext` into references to `ThisContext`
	5) Rewriting references to `self` into references to `ThisContext receiver`"
	rewriter := OCParseTreeRewriter new.
	semanticallyAnalysedMethodAST := anAST doSemanticAnalysis.	"To find which variables are undefined"
	rewriter
		replace: (OCVariableNode named: #ThisContext) name
		with:
			'(ThisContext readVariableNamed: #'
				, (OCVariableNode named: #ThisContext) name , ')'.
	allMessageNodes := semanticallyAnalysedMethodAST allChildren
		select: [ :astElem | astElem isMessage ].
	receiverSuperclassValueNode := self parserClass
		parseExpression: 'ThisContext receiver class superclass'.
	allMessageNodes
		do: [ :msgNode |
			msgNode receiver isSuperVariable
				ifTrue: [ rewriter
						replaceTree: msgNode
						withTree:
							(OCMessageNode
								receiver: (self parserClass parseExpression: 'ThisContext receiver')
								selector: #perform:withArguments:inSuperclass:
								arguments:
									{(OCLiteralValueNode value: msgNode selector).
									(OCArrayNode statements: msgNode arguments).
									receiverSuperclassValueNode}) ] ].
	intermediateAST := rewriter
		executeTree: semanticallyAnalysedMethodAST;
		tree.
	analysedIntermediateAST := intermediateAST doSemanticAnalysis.
	rewriter2 := OCParseTreeRewriter new.
	allUndeclaredVariableNodes := analysedIntermediateAST allChildren
		select: [ :astElem | astElem isVariable and: [ astElem isUndeclaredVariable ] ].
	allUndeclaredVariableSymbols := (allUndeclaredVariableNodes
		collect: [ :varNode | varNode name ]) asSet asArray.
	allUndeclaredVariableSymbols
		withIndexDo: [ :tempName :index |
			rewriter2
				replace: tempName
				with: '(ThisContext readVariableNamed: #' , tempName , ')' ].
	rewriter2 replace: 'thisContext' with: 'ThisContext'.
	rewriter2 replace: 'self' with: 'ThisContext receiver'.
	rewrittenConditionBlockProducerAST := rewriter2
		executeTree: analysedIntermediateAST;
		tree.
	^ rewrittenConditionBlockProducerAST doSemanticAnalysis
]

{ #category : 'api' }
Breakpoint >> scopeTo: anInstance [

	self targetInstance: anInstance
]

{ #category : 'install' }
Breakpoint >> setAsNodeProperty [

	self node addBreakpoint: self
]

{ #category : 'accessing' }
Breakpoint >> targetInstance [

	^ targetInstance
]

{ #category : 'accessing' }
Breakpoint >> targetInstance: anObject [

	targetInstance := anObject
]
