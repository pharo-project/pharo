reporting
reportVMParameters: aStream
	| vmParameters |
	self header: 'Virtual Machine Parameters' on: aStream.
	vmParameters := Smalltalk vm getParameters.
	#(
		1	'end of old-space (0-based, read-only)'
		2	'end of young-space (read-only)'
		3	'end of memory (read-only)'
		4	'allocationCount (read-only)'
		5	'allocations between GCs (read-write)'
		6	'survivor count tenuring threshold (read-write)'
		7	'full GCs since startup (read-only)'
		8	'total milliseconds in full GCs since startup (read-only)'
		9	'incremental GCs since startup (read-only)'
		10	'total milliseconds in incremental GCs since startup (read-only)'
		11	'tenures of surving objects since startup (read-only)'
		12	'specific to the translating VM'
		13  'specific to the translating VM'
		14  'specific to the translating VM'
		15  'specific to the translating VM'
		16  'specific to the translating VM'
		17  'specific to the translating VM'
		18  'specific to the translating VM'
		19  'specific to the translating VM'
		20  'specific to the translating VM'
		21	'root table size (read-only)'
		22	'root table overflows since startup (read-only)'
		23	'bytes of extra memory to reserve for VM buffers, plugins, etc.'
		24	'memory threshold above which shrinking object memory (rw)'
		25	'memory headroom when growing object memory (rw)'
		26  'interruptChecksEveryNms - force an ioProcessEvents every N milliseconds, in case the image  is not calling getNextEvent often (rw)'
		27	'number of times mark loop iterated for current IGC/FGC (read-only) includes ALL marking'
		28	'number of times sweep loop iterated  for current IGC/FGC (read-only)'
		29	'number of times make forward loop iterated for current IGC/FGC (read-only)'
		30	'number of times compact move loop iterated for current IGC/FGC (read-only)'
		31	'number of grow memory requests (read-only)'
		32	'number of shrink memory requests (read-only)'
		33	'number of root table entries used for current IGC/FGC (read-only)'
		34	'number of allocations done before current IGC/FGC (read-only)'
		35	'number of survivor objects after current IGC/FGC (read-only)'
		36  'millisecond clock when current IGC/FGC completed (read-only)'
		37  'number of marked objects for Roots of the world, not including Root Table entries for current IGC/FGC (read-only)'
		38  'milliseconds taken by current IGC  (read-only)'
		39  'Number of finalization signals for Weak Objects pending when current IGC/FGC completed (read-only)'
		40  'VM word size - 4 or 8 (read-only)'
	) pairsDo: [:idx :desc |
		aStream
			nextPut: $#;
			nextPutAll: idx printString;
			tab;
			nextPutAll: (vmParameters at: idx) printString;
			tab;
			nextPutAll: desc;
			cr]