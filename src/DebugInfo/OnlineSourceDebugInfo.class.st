"
I represent the Debug API of methods/closures.
My main methods, in the `api` protocol, allow the user (debugger, inspector) to read variables from a context, and to obtain bytecode to source code mappings:
  - is the current PC a send?
  - is the current PC a store?
  ...

I'm an online implementation: I compute the debug information on the fly using the compiler.
In other words, I only work if the compiler is available.

Using `asOfflineDebugInfo` I transform myself into an `OfflineSourceDebugInfo` that does not require a compiler.
Check the comment of `OfflineSourceDebugInfo` for more information.

## Implementation details

Internally I compute scope objects for optimized lexical scopes.
Optimized scopes appear when the compiler inlines a closure inside a method.
In that case, I know how to give users ""the impression"" of being in a non-optimized world.
For this purpose, scopes know:
 - what variables are accessible at a certain scope (bytecode range)
 - how should they be accessed (how many contexts should be looked up in the sender chain, and how to read the variable from the context -- considering temp vectors)
"
Class {
	#name : 'OnlineSourceDebugInfo',
	#superclass : 'Object',
	#instVars : [
		'compiledCode',
		'bytecodeToASTCache'
	],
	#category : 'DebugInfo',
	#package : 'DebugInfo'
}

{ #category : 'instance creation' }
OnlineSourceDebugInfo class >> for: code [

	^ self new
		  compiledCode: code;
		  yourself
]

{ #category : 'private' }
OnlineSourceDebugInfo >> asBitMask: aCollectionOfFlags [

	| flags |
	self assert: aCollectionOfFlags size < SmallInteger maxVal highBit.
	flags := 0.
	aCollectionOfFlags
		do: [ :f | flags := flags + f asBit ]
		separatedBy: [ flags := flags << 1 ].
	^ flags
]

{ #category : 'converting' }
OnlineSourceDebugInfo >> asOfflineDebugInfo [

	| debugInfo stopTable astScopes scopes bytecodeRange |
	debugInfo := OfflineSourceDebugInfo new.
	debugInfo sourceCode: nil. "self sourceCode"

	stopTable := compiledCode symbolicBytecodes asArray select: [ :e | self shouldStopAt: e offset ] thenCollect: [ :e |
			             | range stopTableInfo |
			             range := self rangeForPC: e offset.
			             stopTableInfo := {
				                              e offset.
				                              (self asBitMask: {
						                               (self isSourceStore: e offset).
						                               (self isSourceReturn: e offset).
						                               (self isSourceSend: e offset) }).
				                              range first.
				                              range last }.
			             stopTableInfo ]. "Compress the stop table as a byte array if possible"
	stopTable := stopTable flattened.
	(stopTable allSatisfy: [ :byte | byte < 255 ]) ifTrue: [ stopTable := stopTable asByteArray ].
	debugInfo stopTable: stopTable.

	astScopes := (OCScopesCollector new visitNode: compiledCode ast) scopes.

	scopes := Dictionary new.
	astScopes do: [ :astScope |
			| onlineScope cachedScope |
			onlineScope := OnlineSourceDebugScope on: astScope.
			cachedScope := OfflineSourceDebugScope new.
			(astScope isBlockScope and: [ astScope isOptimized ])
				ifTrue: [
						| mappedPCs currentScopeBytecodeToASTCache blockBody blockStatements |
						blockBody := astScope node body.
						blockStatements := blockBody statements.
						currentScopeBytecodeToASTCache := astScope outerNotOptimizedScope node bcToASTCache.
						blockStatements ifEmpty: [
							mappedPCs := currentScopeBytecodeToASTCache pcsForNode: blockBody ] ifNotEmpty: [
								mappedPCs := ((blockStatements flatCollect: [ :e | e withAllChildren ])
									              flatCollect: [ :e | currentScopeBytecodeToASTCache pcsForNode: e ]
									              as: Set) sorted ].
						"It may happen that no children has a mapping.
						This happens when a node is completely optimized out.
						For example, a statement reading a variable but doing nothing else.
						In that case, save an empty range"
						mappedPCs ifNotEmpty: [ bytecodeRange := mappedPCs first to: mappedPCs last ] ]
				ifFalse: [
						| bytecodeCache |
						bytecodeCache := astScope outerNotOptimizedScope node bcToASTCache.
						bytecodeRange := bytecodeCache firstBcOffset to: bytecodeCache lastBcOffset ].

			"If the scope has no bytecode mapping, ignore it"
			bytecodeRange ifNotNil: [ | tempDescriptions |
				cachedScope bytecodeRange: bytecodeRange.
				tempDescriptions := onlineScope variables collect: [ :temp | temp descriptionArrayForScope: astScope ].
				tempDescriptions ifNotEmpty: [ cachedScope variables: tempDescriptions ].
				scopes at: astScope put: cachedScope ] ].

	astScopes do: [ :each | 
		scopes
			at: each outerScope
			ifPresent: [ :parent | parent addChild: (scopes at: each) ] ].
	debugInfo rootScope: (scopes at: compiledCode ast scope).

	^ debugInfo
]

{ #category : 'accessing' }
OnlineSourceDebugInfo >> compiledCode [

	^ compiledCode
]

{ #category : 'accessing' }
OnlineSourceDebugInfo >> compiledCode: anObject [

	compiledCode := anObject
]

{ #category : 'accessing' }
OnlineSourceDebugInfo >> debugScopeAt: aBytecodePC [

	| node scope |
	node := self privateSourceNodeForPC: aBytecodePC.

	scope := node isBlock
		         ifTrue: [ node parent methodOrBlockNode scope ]
		         ifFalse: [ node methodOrBlockNode scope ].

	^ OnlineSourceDebugScope on: scope
]

{ #category : 'testing' }
OnlineSourceDebugInfo >> isArgumentNamed: aName at: aBytecodePC [

	^ (self debugScopeAt: aBytecodePC) isArgumentNamed: aName
]

{ #category : 'private' }
OnlineSourceDebugInfo >> isFirstPCOfMessage: aBytecodePC [

	"Check if the given bytecode PC is the first bytecode PC of its mapped node.
	
	An AST node can be expanded/inlined in many bytecodes.
	But, from a source/user point of view, it does not make sense to stop at each bytecode.
	Instead, let's stop at the first bytecode of the expansion.
	
	The implementation is tricky.
	First we get the AST node for the pc (many pcs can be mapped to the same node).
	Now, a node does not know its expansion.
	It is its (first non-optimized) parent that knows.
	So, fetch its (non-optimized) lexical parent, ask it for the first PC for the mapped node.
	Return if this first PC is the one as argument"
	| mappedNode |
	mappedNode := self privateSourceNodeForPC: aBytecodePC.
	^ (mappedNode methodOrBlockNode scope outerNotOptimizedScope node firstPcForNode: mappedNode) = aBytecodePC
]

{ #category : 'api' }
OnlineSourceDebugInfo >> isSourceBlockCreation: aBytecodePC [
	
	"For many reasons we need for now to consider that the block creation is the block creation bytecode, and not the first bytecode mapped to it -- which correctly considers the pushing of copied vars and temp vectors"
	^ (InstructionStream on: compiledCode pc: aBytecodePC) willCreateBlock

	"(self privateSourceNodeForPC: aBytecodePC) ifNil: [ ^ false ].
	^ (self privateSourceNodeForPC: aBytecodePC) isBlock"
]

{ #category : 'api' }
OnlineSourceDebugInfo >> isSourceJump: aBytecodePC [
	"Returns if the next bytecode will jump"

	^ (InstructionStream on: compiledCode pc: aBytecodePC) willJump
]

{ #category : 'api' }
OnlineSourceDebugInfo >> isSourceReturn: aBytecodePC [

	"We should stop at all returns inserted by the compiler in the bytecode.
	This includes:
	 - explicit returns with ^ -- these are in the AST
	 - implicit block local returns -- not in AST
	 - implicit method returns -- not in AST"
	^ compiledCode encoderClass isReturnAt: aBytecodePC in: compiledCode
]

{ #category : 'api' }
OnlineSourceDebugInfo >> isSourceSend: aBytecodePC [

	"For many reasons we need for now to consider that the sends are the send bytecodes.
	However, this takes into account sends inserted by the compiler and not written by users!"
	^ (InstructionStream on: compiledCode pc: aBytecodePC) willSend

	"Deal with inlined messages using isFirstPCOfMessagePC:"
	"(self privateSourceNodeForPC: aBytecodePC) ifNil: [ ^ false ].
	^ (self privateSourceNodeForPC: aBytecodePC) isMessage and: [
		  self isFirstPCOfMessage: aBytecodePC ]"
]

{ #category : 'api' }
OnlineSourceDebugInfo >> isSourceStore: aBytecodePC [
	
	"For many reasons we need for now to consider that the stores are the store bytecodes.
	However, this takes into account stores inserted by the compiler and not written by users!"
	^ (InstructionStream on: compiledCode pc: aBytecodePC) willStore

	"(self privateSourceNodeForPC: aBytecodePC) ifNil: [ ^ false ].
	^ (self privateSourceNodeForPC: aBytecodePC) isAssignment"
]

{ #category : 'private' }
OnlineSourceDebugInfo >> privateSourceNodeForPC: aBytecodePC [

	| sourceNode |

	"Lazy initialize the bytecode to AST cache"
	bytecodeToASTCache ifNotNil: [
		^ bytecodeToASTCache nodeForPC: aBytecodePC ].

	sourceNode := compiledCode sourceNode.
	bytecodeToASTCache := OCBytecodeToASTCache generateForNode:
		                      sourceNode.
	^ bytecodeToASTCache nodeForPC: aBytecodePC
]

{ #category : 'api' }
OnlineSourceDebugInfo >> rangeForPC: aPC [

	^ (self privateSourceNodeForPC: aPC) debugHighlightRange
]

{ #category : 'api' }
OnlineSourceDebugInfo >> readVariableNamed: aName fromContext: aContext [

    | currentPC |
    "If the context is dead (pc = nil) then we really cannot know how to interpret the temp slots, as it could have been killed in any state.
    For backwards compatibility, assume that the dead context has all the temps available at the beginning of the method -- i.e., at its initial PC"
    currentPC := aContext pc ifNil: [ compiledCode initialPC ].
	^ (self debugScopeAt: currentPC) readVariableNamed: aName fromContext: aContext
]

{ #category : 'api' }
OnlineSourceDebugInfo >> shouldStopAt: aBytecodePC [

	^ (self isSourceSend: aBytecodePC) or: [
			  (self isSourceReturn: aBytecodePC) or: [
				  (self isSourceStore: aBytecodePC) or: [ (self isSourceBlockCreation: aBytecodePC) or: [ self isSourceJump: aBytecodePC ] ] ] ]
]

{ #category : 'accessing' }
OnlineSourceDebugInfo >> sourceCode [
	
	^ compiledCode sourceCode
]

{ #category : 'accessing' }
OnlineSourceDebugInfo >> variableNamesAt: aBytecodePC [

	^ (self debugScopeAt: aBytecodePC) variableNames
]
