performing
performSymbol: aSymbol withArguments: aCollection
	|time|
	"we save current timestamp here, so that defer block can check if it belongs to the correct task"
	time := self timestamp.
	"UI manages commands for us"
	Processor yield.
	"if timestamp is wrong just skip command"
	UIManager default defer: [ time = self timestamp ifTrue: [ self receiver perform: aSymbol withEnoughArguments: aCollection ] ]