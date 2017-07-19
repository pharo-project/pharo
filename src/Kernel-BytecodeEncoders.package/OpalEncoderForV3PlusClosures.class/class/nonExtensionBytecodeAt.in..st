instruction stream support
nonExtensionBytecodeAt: pc in: method
	"Answer the actual bytecode at pc in method, skipping past any preceeding extensions."
	^ method at: pc