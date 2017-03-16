enumerating
entriesDo: aBlockClosure
	"Evaluate aBlockClosure on every entry"
	
	^ self entries do: aBlockClosure