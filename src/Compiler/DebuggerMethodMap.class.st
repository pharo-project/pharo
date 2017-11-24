"
I am a place-holder for information needed by the Debugger to inspect method activations.  I insulate the debugger from details of code generation such as exact bytecode offsets and temporary variable locations. 

My function is to abstract the source map away from actual bytecode pcs to abstract bytecode pcs.

To reduce compilation time I try and defer as much computation to access time as possible as instances of me will be created after each compilation.

I maintain a WeakIdentityDictionary of method to DebuggerMethodMap to cache maps.  I refer to my method through a WeakArray to keep the map cache functional. If the reference from a DebuggerMethodMap to its method were strong then the method would never be dropped from the cache because the reference from its map would keep it alive.
"
Class {
	#name : #DebuggerMethodMap,
	#superclass : #Object,
	#instVars : [
		'methodReference',
		'methodNode',
		'abstractSourceRanges',
		'sortedSourceMap',
		'blockExtentsToTempRefs',
		'startpcsToTempRefs'
	],
	#category : #'Compiler-Support'
}

{ #category : #'instance creation' }
DebuggerMethodMap class >> forMethod: aMethod [ "<CompiledMethod>"
	"Answer a DebuggerMethodMap suitable for debugging activations of aMethod"
	^self new forMethod: aMethod
	
				
]

{ #category : #'source mapping' }
DebuggerMethodMap >> abstractPCFor: concretePC when: contextIsActive [
	"If the context is the actve context (is at the hot end of the stack)
	 then its pc is the current pc.  But if the context isn't, because it is
	suspended sending a message, then its current pc is the previous pc"
	^self  abstractPCForConcretePC:
		(contextIsActive 
			ifTrue: [concretePC]
			ifFalse: [(self method pcPreviousTo: concretePC) ifNil: [concretePC]]) method: self method
]

{ #category : #'source mapping' }
DebuggerMethodMap >> abstractPCForConcretePC: concretePC method: aMethod [
	"Answer the abstractPC matching concretePC."

	| abstractPC scanner client |
	abstractPC := 1.
	scanner := InstructionStream on: aMethod.
	client := InstructionClient new.
	[(scanner atEnd
	  or: [scanner pc >= concretePC]) ifTrue:
		[^abstractPC].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 true] whileTrue
]

{ #category : #'source mapping' }
DebuggerMethodMap >> abstractSourceMap [
	"Answer with a Dictionary of abstractPC <Integer> to sourceRange <Interval>."
	| theMethodToScan rawSourceRanges concreteSourceRanges abstractPC scanner client |
	abstractSourceRanges ifNotNil:
		[^abstractSourceRanges].
	"If the methodNode hasn't had a method generated it doesn't have pcs set in its
	 nodes so we must generate a new method and might as well use it for scanning."
	methodNode rawSourceRangesAndMethodDo:
		[:ranges :method|
		 rawSourceRanges := ranges.
		 theMethodToScan := method].
	concreteSourceRanges := Dictionary new.
	rawSourceRanges keysAndValuesDo:
		[:node :range|
		node pc ~= 0 ifTrue:
			[concreteSourceRanges at: node pc put: range]].
	abstractPC := 1.
	abstractSourceRanges := Dictionary new.
	scanner := InstructionStream on: theMethodToScan.
	client := InstructionClient new.
	[(concreteSourceRanges includesKey: scanner pc) ifTrue:
		[abstractSourceRanges at: abstractPC put: (concreteSourceRanges at: scanner pc)].
	 abstractPC := abstractPC + 1.
	 scanner interpretNextInstructionFor: client.
	 scanner atEnd] whileFalse.
	^abstractSourceRanges
]

{ #category : #private }
DebuggerMethodMap >> blockExtentsInto: aDictionary from: initialPC to: endPC scanner: scanner numberer: numbererBlock [
	"Support routine for startpcsToBlockExtents"
	| extentStart blockSizeOrLocator |
	extentStart := numbererBlock value.
	[scanner pc <= endPC] whileTrue:
		[blockSizeOrLocator := scanner interpretNextInstructionFor: BlockStartLocator new.
		 blockSizeOrLocator isInteger ifTrue:
			[self
				blockExtentsInto: aDictionary
				from: scanner pc
				to: scanner pc + blockSizeOrLocator - 1
				scanner: scanner
				numberer: numbererBlock]].
	aDictionary at: initialPC put: (extentStart to: numbererBlock value).
	^aDictionary
]

{ #category : #'initialize-release' }
DebuggerMethodMap >> forMethod: aMethod [ 
	^self forMethod: aMethod methodNode: aMethod methodNode.
]

{ #category : #'initialize-release' }
DebuggerMethodMap >> forMethod: aMethod "<CompiledMethod>" methodNode: theMethodNode [ "<MethodNode>"
	methodReference := WeakArray with: aMethod.
	methodNode := theMethodNode.
]

{ #category : #private }
DebuggerMethodMap >> mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString method: aCompiledMethod [
	"Decode a schematicTempNamesString that encodes the layout of temp names
	 in a method and any closures/blocks within it, matching keys in keys to
	 vectors of temp names."
	| map tempNames |
	map := Dictionary new.
	tempNames := schematicTempNamesString readStream.
	keys do:
		[:key| | tempSequence tempIndex |
		tempSequence := OrderedCollection new.
		tempIndex := 0.
		[(tempNames skipSeparators; peek) ifNil: [true] ifNotNil: [:ch| '[]' includes: ch]] whileFalse:
			[tempNames peek = $(
				ifTrue: [tempSequence addAllLast: ((self tempsSubSequenceFrom: (tempNames next; yourself)) withIndexCollect:
														[:temp :index|
														{ temp. { tempIndex + 1. index } }]).
						tempNames peek ~= $) ifTrue: [self error: 'parse error'].
						tempIndex := tempIndex + 1.
						tempNames next]
				ifFalse: [tempSequence addAllLast: ((self tempsSubSequenceFrom: tempNames) withIndexCollect:
														[:temp :index|
														{ temp. tempIndex := tempIndex + 1 }])]].
		map at: key put: tempSequence asArray.
		[tempNames peek = $]] whileTrue: [tempNames next].
		tempNames peek = $[ ifTrue:
			[tempNames next]].
	^map
]

{ #category : #accessing }
DebuggerMethodMap >> method [
	^methodReference at: 1
]

{ #category : #accessing }
DebuggerMethodMap >> namedTempAt: index in: aContext [
	"Answer the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:"
	^self
		privateTempAt: index
		in: aContext
		startpcsToBlockExtents: (self startpcsToBlockExtents: aContext method)
]

{ #category : #accessing }
DebuggerMethodMap >> namedTempAt: index put: aValue in: aContext [
	"Assign the value of the temp at index in aContext where index is relative
	 to the array of temp names answered by tempNamesForContext:.
	 If the value is a copied value we also need to set it along the lexical chain."
	^self
		privateTempAt: index
		in: aContext
		put: aValue
		startpcsToBlockExtents: (self startpcsToBlockExtents: aContext method)
]

{ #category : #private }
DebuggerMethodMap >> privateDereference: tempReference in: aContext [
	"Fetch the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue: [aContext tempAt: tempReference]
		ifFalse:
			[tempReference first == #outer
				ifTrue: [self privateDereference: tempReference last
							in: aContext outerContext]
				ifFalse: [(aContext tempAt: tempReference first)
							at: tempReference second]]
]

{ #category : #private }
DebuggerMethodMap >> privateDereference: tempReference in: aContext put: aValue [
	"Assign the temporary with reference tempReference in aContext.
	 tempReference can be
		integer - direct temp reference
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index
		#( outer. temp reference ) - a temp reference in an outer context."
	^tempReference isInteger
		ifTrue: [aContext tempAt: tempReference put: aValue]
		ifFalse:
			[tempReference first == #outer
				ifTrue: [self privateDereference: tempReference last
							in: aContext outerContext
							put: aValue]
				ifFalse: [(aContext tempAt: tempReference first)
							at: tempReference second
							put: aValue]]
]

{ #category : #private }
DebuggerMethodMap >> privateIsOuter: anObject [ 
	^anObject last isArray and: [anObject last first == #outer]
]

{ #category : #private }
DebuggerMethodMap >> privateTempAt: index in: aContext put: aValue startpcsToBlockExtents: theContextsStartpcsToBlockExtents [
	| nameRefPair |
	nameRefPair := (self privateTempRefsForContext: aContext
						 startpcsToBlockExtents: theContextsStartpcsToBlockExtents)
						at: index
						ifAbsent: [aContext errorSubscriptBounds: index].
	^self privateDereference: nameRefPair last in: aContext put: aValue
]

{ #category : #private }
DebuggerMethodMap >> privateTempAt: index in: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents [
	| nameRefPair |
	nameRefPair := (self privateTempRefsForContext: aContext
						 startpcsToBlockExtents: theContextsStartpcsToBlockExtents)
						at: index
						ifAbsent: [aContext errorSubscriptBounds: index].
	^self privateDereference: nameRefPair last in: aContext
]

{ #category : #private }
DebuggerMethodMap >> privateTempRefsForContext: aContext startpcsToBlockExtents: theContextsStartpcsToBlockExtents [
	"Answer the sequence of temps in scope in aContext in the natural order,
	 outermost arguments and temporaries first, innermost last.  Each temp is
	 a pair of the temp's name followed by a reference.  The reference can be
		integer - index of temp in aContext
		#( indirectionVectorIndex tempIndex ) - remote temp in indirectionVector at index in aContext
		#( outer. temp reference ) - a temp reference in an outer context."
	blockExtentsToTempRefs ifNil:
		[blockExtentsToTempRefs := methodNode blockExtentsToTempsMap.
		 startpcsToTempRefs := Dictionary new].
	^startpcsToTempRefs
		at: aContext startpc
		ifAbsentPut:
			[| localRefs |
			 localRefs := blockExtentsToTempRefs at: (theContextsStartpcsToBlockExtents at: aContext startpc).
			 aContext outerContext
				ifNil: [localRefs]
				ifNotNil:
					[:outer| | outerTemps |
					"Present temps in the order outermost to innermost left-to-right, but replace
					 copied outermost temps with their innermost copies"
					 outerTemps := (self
										privateTempRefsForContext: outer
										startpcsToBlockExtents: theContextsStartpcsToBlockExtents) collect:
						[:outerPair|
						localRefs
							detect: [:localPair| outerPair first = localPair first]
							ifNone: [{ outerPair first. { #outer. outerPair last } }]].
					outerTemps,
					 (localRefs reject: [:localPair| outerTemps anySatisfy: [:outerPair| localPair first = outerPair first]])]]
]

{ #category : #'source mapping' }
DebuggerMethodMap >> rangeForPC: concretePC contextIsActiveContext: contextIsActive [
  	"Answer the indices in the source code for the supplied pc."
	
	| pc |
	
	self sortedSourceMap 
		ifEmpty: [ ^(1 to: 0) ].

	pc := self abstractPCFor: concretePC when: contextIsActive.

	(self sortedSourceMap first key > pc)
		ifTrue: [ ^self sortedSourceMap first value ].
	
	^self abstractSourceMap 
		at: pc 
		ifAbsent: [
			(self sortedSourceMap 
				findBinary: [:assoc | pc - assoc key ] 
				ifNone: [ | end |
					end := self sortedSourceMap last value last.
					nil -> (end +1 to: end)]) value ]
]

{ #category : #'source mapping' }
DebuggerMethodMap >> sortedSourceMap [
	"Answer a sorted collection of associations, pcRangeStart -> pcRangeInterval "
	^ sortedSourceMap
		ifNil: [sortedSourceMap := self abstractSourceMap associations sorted]
]

{ #category : #private }
DebuggerMethodMap >> startpcsToBlockExtents: aCompiledMethod [
	"Answer a Dictionary of startpc to Interval of blockExtent, using the
	 identical numbering scheme described in and orchestrated by
	 BlockNode>>analyseArguments:temporaries:rootNode:.  This is
	 used in part to find the temp names for any block in a method, as
	 needed by the debugger.  The other half is to recompile the method,
	 obtaining the temp names for each block extent.  By indirecting through
	 the blockExtent instead of using the startpc directly we decouple the
	 debugger's access to temp names from the exact bytecode; insulating
	 debugging from minor changes in the compiler (e.g. changes in literal
	 pooling, adding prefix bytecodes, adding inst vars to CompiledMethod
	 in literals towards the end of the literal frame, etc).  If the recompilation
	 doesn't produce exactly the same bytecode at exactly the same offset
	 no matter; the blockExtents will be the same."
	| index |
	index := 0.
	^self
		blockExtentsInto: Dictionary new
		from: aCompiledMethod initialPC
		to: aCompiledMethod endPC
		scanner: (InstructionStream on: aCompiledMethod)
		numberer: [| value | value := index. index := index + 2. value] 
]

{ #category : #accessing }
DebuggerMethodMap >> tempNamesForContext: aContext [
	"Answer an Array of all the temp names in scope in aContext starting with
	 the home's first local (the first argument or first temporary if no arguments)."
	^(self
		privateTempRefsForContext: aContext
		startpcsToBlockExtents: (self startpcsToBlockExtents: aContext method)) collect:
			[:pair| pair first]
]

{ #category : #private }
DebuggerMethodMap >> tempsSubSequenceFrom: tempNamesStream [
	^Array streamContents:
		[:tsss|
		[tempNamesStream skipSeparators.
		 tempNamesStream atEnd
		 or: ['[]()' includes: tempNamesStream peek]] whileFalse:
			[tsss nextPut: (String streamContents:
							[:s|
							[s nextPut: tempNamesStream next.
							 tempNamesStream peek
								ifNil: [true]
								ifNotNil: [:peek| ' []()' includes: peek]] whileFalse])]]

	"thisContext method tempsSubSequenceFrom: 'les temps perdu(sont n''est pas la)' readStream"
	"thisContext method tempsSubSequenceFrom: ('les temps perdu(sont n''est pas la)' readStream skipTo: $(; yourself)"
]
