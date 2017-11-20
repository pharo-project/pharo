"
I encode names and literals into tree nodes with byte codes for the compiler. Byte codes for literals are not assigned until the tree-sizing pass of the compiler, because only then is it known which literals are actually needed. I also keep track of sourceCode ranges during parsing and code generation so I can provide an inverse map for the debugger.
"
Class {
	#name : #LegacyEncoder,
	#superclass : #ParseNode,
	#instVars : [
		'scopeTable',
		'nTemps',
		'supered',
		'requestor',
		'class',
		'selector',
		'literalStream',
		'selectorSet',
		'litIndSet',
		'litSet',
		'sourceRanges',
		'globalSourceRanges',
		'addedSelectorAndMethodClassLiterals',
		'optimizedSelectors'
	],
	#category : #Compiler-Kernel
}

{ #category : #visiting }
LegacyEncoder >> accept: aVisitor [
	"I am not really a ParseNode.  Only here to access constants defined in parseNode."
	self shouldNotImplement
]

{ #category : #results }
LegacyEncoder >> allLiterals [
	addedSelectorAndMethodClassLiterals ifFalse:
		[addedSelectorAndMethodClassLiterals := true.
		"Put the optimized selectors in literals so as to browse senders more easily"
		optimizedSelectors := optimizedSelectors reject: [:e| literalStream originalContents hasLiteral: e].
		optimizedSelectors isEmpty ifFalse: [
			"Use one entry per literal if enough room, else make anArray"
			literalStream position + optimizedSelectors size + 2 > 255
				ifTrue: [self litIndex: optimizedSelectors asArray]
				ifFalse: [optimizedSelectors do: [:e | self litIndex: e]]].
		"Add a slot for selector or MethodProperties"
		self litIndex: nil.
		self litIndex: self associationForClass].
	^literalStream contents
]

{ #category : #results }
LegacyEncoder >> associationForClass [
	| assoc |
	assoc := self environment associationAt: class name ifAbsent: [nil].
	^assoc value == class
		ifTrue: [assoc]
		ifFalse: [Association new value: class]
]

{ #category : #temps }
LegacyEncoder >> autoBind: name [ 
	"Declare a block argument as a temp if not already declared."
	| node |
	node := scopeTable 
			at: name
			ifAbsent: 
				[(self lookupInPools: name ifFound: [:assoc | assoc])
					ifTrue: [self warnAboutShadowed: name].
				^ (self reallyBind: name) nowHasDef nowHasRef scope: 1].
	node isTemp
		ifTrue: [node scope >= 0 ifTrue:
					[^ self notify: 'Name already used in this method'].
				node nowHasDef nowHasRef scope: 1]
		ifFalse: [^ self notify: 'Name already used in this class'].
	^node
]

{ #category : #temps }
LegacyEncoder >> bindAndJuggle: name [

	| node nodes first thisCode |
	node := self reallyBind: name.

	"Declared temps must precede block temps for decompiler and debugger to work right"
	nodes := self tempNodes.
	(first := nodes findFirst: [:n | n scope > 0]) > 0 ifTrue:
		[node == nodes last ifFalse: [self error: 'logic error'].
		thisCode := (nodes at: first) code.
		first to: nodes size - 1 do:
			[:i | (nodes at: i) key: (nodes at: i) key
							code: (nodes at: i+1) code].
		nodes last key: nodes last key code: thisCode].
	
	^ node
]

{ #category : #temps }
LegacyEncoder >> bindArg: name [ 
	"Declare an argument."
	| node |
	nTemps >= 15
		ifTrue: [^self notify: 'Too many arguments'].
	node := self bindTemp: name.
	^ node nowHasDef nowHasRef
]

{ #category : #temps }
LegacyEncoder >> bindBlockArg: name within: aBlockNode [
	"With standard Smalltalk-80 (BlueBook) blocks it used to be legal to use a
	 method temp as a block argument.  This shouldn't be the case with the
	 current compiler, which checks for temp names already being used as
	 block arguments.  But it is easily fooled by local block temps in optimized
	 blocks, e.g.
		false
			ifTrue: [| temp |]
			ifFalse:[[:temp|]]
	Rather than fix this we keep the semantics and fix it in the closure compiler."
	^self autoBind: name
]

{ #category : #temps }
LegacyEncoder >> bindBlockTemp: name [ 
	"Declare a temporary block variable; complain if it's not a field or class variable."

	| node |

	node := scopeTable at: name ifAbsent: [^self reallyBind: name].
	node isTemp
		ifTrue: [
			node scope >= 0 ifTrue: [^ self notify: 'Name already used in this method'].
			node scope: 0]
		ifFalse: [^self notify: 'Name already used in this class'].
	^node

]

{ #category : #temps }
LegacyEncoder >> bindBlockTemp: name within: aBlockNode [
	"The BlockContext compiler (the Smalltalk-80 BlueBook compiler)
	 does provide support for ANSI block syntax, but not for ANSI block
	 semantics.  Here all temps live at the same level, the method level.
	 The approach taken to two block-local temps in different blocks is to
	 merge them into a single temp.  e.g.
		expr
			ifTrue: [|temp| self statementOne]
			ifFalse: [|temp| self statementTwo]
	 is effectvely transformed into
		| temp |
		expr
			ifTrue: [self statementOne]
			ifFalse: [self statementTwo]
	 and
		expr do: [:each| | temp | ...].
		expr do: [:each| | temp | ...].
	 is also effectively transformed into
		| temp |
		expr do: [:each|  ...].
		expr do: [:each| ...].

	 The closure compiler treats the former similarly, but not the latter.
	 The indirection through #bindBlockTemp:within: allows the closure encoder to do this."
	^self bindBlockTemp: name
]

{ #category : #temps }
LegacyEncoder >> bindTemp: name [ 
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:
	[:node|
			(requestor interactive)
						ifTrue: [self evaluateShadowingInteractivelyOf: node]
						ifFalse: [self evaluateShadowingNotInteractivelyOf: node] ].
	^self reallyBind: name
]

{ #category : #temps }
LegacyEncoder >> bindTemp: name in: methodSelector [
	"Declare a temporary; error not if a field or class variable."
	scopeTable at: name ifPresent:[:node|
		"When non-interactive raise the error only if its a duplicate"
		(node isTemp or:[requestor interactive])
			ifTrue:[^self notify:'Name is already defined']
			ifFalse:[Transcript 
				show: '(', name, ' is shadowed in "' , class printString , '>>' , methodSelector printString , '")']].
	^self reallyBind: name
]

{ #category : #encoding }
LegacyEncoder >> cantStoreInto: varName [

	^StdVariables includesKey: varName
]

{ #category : #private }
LegacyEncoder >> classEncoding [
	"This is a hack so that the parser may findout what class it was parsing for when it wants to create a syntax error view."
	^ class
]

{ #category : #encoding }
LegacyEncoder >> doItInContextName [
	^'ThisContext'
]

{ #category : #encoding }
LegacyEncoder >> encodeLiteral: object [

	^self
		name: object
		key: (class literalScannedAs: object notifying: self)
		class: LiteralNode
		type: LdLitType
		set: litSet
]

{ #category : #encoding }
LegacyEncoder >> encodeSelector: aSelector [

	^self
		name: aSelector
		key: aSelector
		class: SelectorNode
		type: SendType
		set: selectorSet
]

{ #category : #encoding }
LegacyEncoder >> encodeVariable: name [
	^ self encodeVariable: name sourceRange: nil ifUnknown: [ self undeclared: name ]
]

{ #category : #encoding }
LegacyEncoder >> encodeVariable: name sourceRange: range ifUnknown: action [
	| varNode |
	varNode := scopeTable at: name
			ifAbsent: 
				[(self lookupInPools: name 
					ifFound: [:assoc | varNode := self global: assoc name: name])
					ifTrue: [varNode]
					ifFalse: [^action value]].
	range ifNotNil: [
		name first canBeGlobalVarInitial ifTrue:
			[globalSourceRanges addLast: { name. range. false }]. ].

	(varNode isTemp and: [varNode scope < 0]) ifTrue: [
		"every client just does resume: true"
		action value.
		
	"	OutOfScopeNotification signal 
			ifTrue: [action value]
			ifFalse: [ ^self notify: 'out of scope']."
	].
	^ varNode
]

{ #category : #encoding }
LegacyEncoder >> environment [
	"Answer the environment of the current compilation context,
	 be it in a class or global (e.g. a workspace)"
	^class == nil
		ifTrue: [Smalltalk globals]
		ifFalse: [class environment]
]

{ #category : #temps }
LegacyEncoder >> evaluateShadowingInteractivelyOf: aNode [ 
	self isAnalyzedMethodDefinedByATrait ifTrue: [^false].
	aNode isTemp
				ifTrue: [ (aNode scope >= 0) ifTrue: [ ^self notify:'Name is already defined']  ]
				ifFalse: [ ^self notify:'Name is already defined' ].

]

{ #category : #temps }
LegacyEncoder >> evaluateShadowingNotInteractivelyOf: aNode [ 
	self isAnalyzedMethodDefinedByATrait ifTrue: [^false].
	aNode isTemp
				ifTrue: [(aNode scope >= 0) ifTrue: [ ^self notify:'Name is already defined'] ]
				ifFalse: [ self warnAboutShadowed: aNode name ]

]

{ #category : #initialize-release }
LegacyEncoder >> fillDict: dict with: nodeClass mapping: keys to: codeArray [
	| codeStream |
	codeStream := codeArray readStream.
	keys do: 
		[:key | dict 
				at: key
				put:  (nodeClass new name: key key: key code: codeStream next)]
]

{ #category : #temps }
LegacyEncoder >> fixTemp: name [
	| node |
	node := scopeTable at: name ifAbsent: [].
	node class ~~ TempVariableNode ifTrue:
		[self error: 'can only fix a floating temp var'].
	node index: nTemps.
	nTemps := nTemps + 1.
	^node
]

{ #category : #temps }
LegacyEncoder >> floatTemp: node [
	(node ~~ (scopeTable at: node name ifAbsent: [])
	or: [node class ~~ TempVariableNode
	or: [node code ~= (node code: nTemps - 1 type: LdTempType)]]) ifTrue:
		[self error: 'can only float the last allocated temp var'].
	nTemps := nTemps - 1
]

{ #category : #private }
LegacyEncoder >> global: ref name: name [

	^self
		name: name
		key: ref
		class: LiteralVariableNode
		type: LdLitIndType
		set: litIndSet
]

{ #category : #initialize-release }
LegacyEncoder >> init: aClass context: aContext notifying: req [
	requestor := req.
	class := aClass.
	nTemps := 0.
	supered := false.
	self initScopeAndLiteralTables.
	class variablesAndOffsetsDo:
		[:variable "<String>" :offset "<Integer>" |
			scopeTable
						at: variable
						put: (offset >= 0
								ifTrue: [InstanceVariableNode new
											name: variable index: offset]
								ifFalse: [MaybeContextInstanceVariableNode new
											name: variable index: offset negated])].
	aContext ~~ nil ifTrue:
		[| homeNode |
		 homeNode := self bindTemp: self doItInContextName.
		 "0th temp = aContext passed as arg"
		 aContext tempNames withIndexDo:
			[:variable :index|
			scopeTable
				at: variable
				put: (MessageAsTempNode new
						receiver: homeNode
						selector: #namedTempAt:
						arguments: (Array with: (self encodeLiteral: index))
						precedence: 3
						from: self)]].
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32
]

{ #category : #initialize-release }
LegacyEncoder >> initScopeAndLiteralTables [

	scopeTable := StdVariables copy.
	litSet := StdLiterals copy.
	"comments can be left hanging on nodes from previous compilations.
	 probably better than this hack fix is to create the nodes afresh on each compilation."
	scopeTable do:
		[:varNode| varNode comment: nil].
	litSet do:
		[:varNode| varNode comment: nil].
	selectorSet := StdSelectors copy.
	litIndSet := Dictionary new: 16.
	literalStream := (Array new: 32) writeStream.
	addedSelectorAndMethodClassLiterals := false.
	optimizedSelectors := Set new
]

{ #category : #private }
LegacyEncoder >> interactive [
	^requestor interactive
]

{ #category : #temps }
LegacyEncoder >> isAnalyzedMethodDefinedByATrait [
	^( class traitOrClassOfSelector: #testReplaceFromToWithStartingAt ) isTrait
]

{ #category : #encoding }
LegacyEncoder >> litIndex: literal [
	| p |
	p := literalStream position.
	p = 256 ifTrue:
		[self notify: 'More than 256 literals referenced. 
You must split or otherwise simplify this method.
The 257th literal is: ', literal printString. ^nil].
		"Would like to show where it is in the source code, 
		 but that info is hard to get."
	literalStream nextPut: literal.
	^ p
]

{ #category : #results }
LegacyEncoder >> literals [
	"Should only be used for decompiling primitives"
	^ literalStream contents
]

{ #category : #private }
LegacyEncoder >> lookupInPools: varName ifFound: assocBlock [

	^Symbol
		hasInterned: varName
		ifTrue:
			[:sym|
			(class bindingOf: sym)
				ifNil: [^false]
				ifNotNil: [:assoc| assocBlock value: assoc]]
]

{ #category : #temps }
LegacyEncoder >> maxTemp [

	^nTemps
]

{ #category : #accessing }
LegacyEncoder >> methodNodeClass [
	^MethodNode
]

{ #category : #private }
LegacyEncoder >> name: name key: key class: leafNodeClass type: type set: dict [ 
	^dict at: key
		ifAbsentPut: 
			[leafNodeClass new 
				name: name
				key: key
				index: nil
				type: type]
]

{ #category : #temps }
LegacyEncoder >> newTemp: name [

	nTemps := nTemps + 1.
	^ TempVariableNode new
		name: name
		index: nTemps - 1
		type: LdTempType
		scope: 0
]

{ #category : #encoding }
LegacyEncoder >> noteOptimizedSelector: aSymbol [
	"Register a selector as being optimized.
	These optimized selectors will later be registered into the literals so that tools can easily browse senders."
	optimizedSelectors add: aSymbol
]

{ #category : #'source mapping' }
LegacyEncoder >> noteSourceRange: range forNode: node [

	sourceRanges at: node put: range
]

{ #category : #initialize-release }
LegacyEncoder >> noteSuper [

	supered := true
]

{ #category : #'error handling' }
LegacyEncoder >> notify: string [
	"Put a separate notifier on top of the requestor's window"
	| req |
	requestor == nil
		ifTrue:
			[super notify: string]
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string].
	^false
]

{ #category : #'error handling' }
LegacyEncoder >> notify: string at: location [

	| req |
	requestor == nil
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string at: location].
	^false
]

{ #category : #private }
LegacyEncoder >> possibleVariablesFor: proposedVariable [

	| results |
	results := proposedVariable correctAgainstDictionary: scopeTable
								continuedFrom: nil.
	proposedVariable first canBeGlobalVarInitial ifTrue:
		[ results := class possibleVariablesFor: proposedVariable
						continuedFrom: results ].
	^ proposedVariable correctAgainst: nil continuedFrom: results.

]

{ #category : #'source mapping' }
LegacyEncoder >> rawSourceRanges [

	^ sourceRanges 
]

{ #category : #private }
LegacyEncoder >> reallyBind: name [

	| node |
	node := self newTemp: name.
	scopeTable at: name put: node.
	^node
]

{ #category : #initialize-release }
LegacyEncoder >> release [

	requestor := nil
]

{ #category : #'error handling' }
LegacyEncoder >> requestor: req [
	"Often the requestor is a BrowserCodeController"
	requestor := req
]

{ #category : #accessing }
LegacyEncoder >> selector [
	^selector
]

{ #category : #accessing }
LegacyEncoder >> selector: aSymbol [
	selector := aSymbol
]

{ #category : #encoding }
LegacyEncoder >> sharableLitIndex: literal [
	"Special access prevents multiple entries for post-allocated super send special selectors"
	1 to: literalStream position do:
		[:index|
		(litSet literalEquality: literal and: (literalStream originalContents at: index)) ifTrue:
			[^index - 1]].
	^self litIndex: literal
]

{ #category : #results }
LegacyEncoder >> tempNames [ 

	^ self tempNodes collect:
		[:node | (node isMemberOf: MessageAsTempNode)
					ifTrue: [scopeTable keyAtValue: node]
					ifFalse: [node key]]
]

{ #category : #results }
LegacyEncoder >> tempNodes [ 
	| tempNodes |
	tempNodes := SortedCollection sortBlock: [:n1 :n2 | n1 code <= n2 code].
	scopeTable associationsDo:
		[:assn |
		assn value isArray
			ifTrue: [assn value do: [:temp| tempNodes add: temp]]
			ifFalse: [assn value isTemp ifTrue: [tempNodes add: assn value]]].
	^tempNodes
]

{ #category : #initialize-release }
LegacyEncoder >> temps: tempVars literals: lits class: cl [ 
	"Decompile."

	supered := false.
	class := cl.
	nTemps := tempVars size.
	tempVars do: [:node | scopeTable at: node name put: node].
	(literalStream := lits readStream) position: lits size.
	sourceRanges := Dictionary new: 32.
	globalSourceRanges := OrderedCollection new: 32.

]

{ #category : #encoding }
LegacyEncoder >> undeclared: name [
	| sym |
	requestor interactive ifTrue:
		[requestor == #error: ifTrue:
			[requestor error: 'Undeclared'].
		 ^self notify: 'Undeclared'].
	"Allow knowlegeable clients to squash the undeclared warning if they want (e.g.
	 Diffing pretty printers that are simply formatting text).  As this breaks
	 compilation it should only be used by clients that want to discard the result
	 of the compilation.  To squash the warning use e.g.
		[Compiler format: code in: class notifying: nil decorated: false]
			on: UndeclaredVariableWarning
			do: [:ex| ex resume: false]"
	sym := name asSymbol.
	^(UndeclaredVariableWarning new name: name selector: selector class: class) signal
		ifTrue:
			[Undeclared at: sym put: nil.
			self global: (Undeclared associationAt: sym) name: sym]
		ifFalse:
			[self global: (Association key: sym) name: sym]
]

{ #category : #results }
LegacyEncoder >> unusedTempNames [ 
	| unused |
	unused := OrderedCollection new.
	scopeTable associationsDo:
		[:assn | | name |
		(assn value isUnusedTemp) ifTrue:
			[name := assn value key.
			 name ~= self doItInContextName ifTrue: [unused add: name]]].
	^ unused
]

{ #category : #private }
LegacyEncoder >> warnAboutShadowed: name [
	requestor addWarning: name,' is shadowed'.
	selector ifNotNil:
		[Transcript cr; show: class name,'>>', selector, '(', name,' is shadowed)']
]
