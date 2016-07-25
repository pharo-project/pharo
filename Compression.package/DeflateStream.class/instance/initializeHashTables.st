initialization
initializeHashTables
	hashHead := WordArray new: 1 << HashBits.
	hashTail := WordArray new: WindowSize.
