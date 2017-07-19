instruction stream support
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	| scanpc byte |
	scanpc := pc.
	[ byte := method at: scanpc.
	 byte >= 224 and: [byte <= 225]] whileTrue: [ scanpc := scanpc + 2 ].
	^ byte