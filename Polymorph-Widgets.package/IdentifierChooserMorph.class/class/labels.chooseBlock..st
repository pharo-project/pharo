instance creation
labels: aCollection chooseBlock: aBlock
	"return a new chooser or nil"
	^ aCollection isEmpty
		ifFalse: [ self new chooseBlock: aBlock; labels: aCollection; yourself].
