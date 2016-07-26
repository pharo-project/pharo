accessing-selection
getSelectionFromModel
	"Answer the model's selection interval."

	^ self getSelectionSelector ifNotNil: [ :s | self model perform: s withEnoughArguments: {self} ]