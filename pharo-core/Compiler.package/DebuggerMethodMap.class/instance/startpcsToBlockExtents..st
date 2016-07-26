private
startpcsToBlockExtents: aCompiledMethod
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