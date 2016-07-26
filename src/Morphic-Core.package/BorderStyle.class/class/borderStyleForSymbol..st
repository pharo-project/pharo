instance creation
borderStyleForSymbol: sym
	"Answer a border style corresponding to the given symbol"

	| aSymbol |
	aSymbol := sym == #none ifTrue: [#simple] ifFalse: [sym].
	^ self perform: aSymbol