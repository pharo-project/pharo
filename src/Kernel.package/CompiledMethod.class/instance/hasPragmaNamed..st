accessing-pragmas & properties
hasPragmaNamed: aSymbol
	^ self pragmas anySatisfy: [ :pragma | pragma keyword = aSymbol ]