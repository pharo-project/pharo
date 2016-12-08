as yet unclassified
writeSnapshot: imageFileName ofTransformedImage: spurHeap headerFlags: headerFlags screenSize: screenSizeInteger
	"The bootstrapped image typically contains a few big free chunks and one huge free chunk.
	 Test snapshot writing and loading by turning the largest non-huge chunks into segment bridges
	 and saving."
	| penultimate ultimate sim |
"	[ spurHeap  flushNewSpace. ]
		on: Halt
		do: [:ex|"
			"suppress halts from the usual suspects (development time halts)"	
"			(#(#fullGC #compactImage  #doScavenge:) includes: ex signalerContext sender selector)
				ifTrue: [ex resume]
				ifFalse: [ex pass]]."
	sim := spurHeap coInterpreter.
	sim bootstrapping: true.
	spurHeap
		setEndOfMemory: spurHeap endOfMemory + spurHeap bridgeSize. "hack; initializeInterpreter: cuts it back by bridgeSize"
	sim initializeInterpreter: 0;
		setImageHeaderFlagsFrom: headerFlags;
		setSavedWindowSize: (800<<16)+600;
		setDisplayForm: nil.
	spurHeap allOldSpaceEntitiesDo: [:e| penultimate := ultimate. ultimate := e].
	"Check that we've left behind the old, pre-pigCompact segmented save"
	self assert: (spurHeap isFreeObject: penultimate) not.
	spurHeap checkFreeSpace.
	spurHeap runLeakCheckerForFullGC.
	sim bereaveAllMarriedContextsForSnapshotFlushingExternalPrimitivesIf: true.
	sim imageName: imageFileName.
	sim writeImageFileIO.
	Transcript cr; show: 'Done!'