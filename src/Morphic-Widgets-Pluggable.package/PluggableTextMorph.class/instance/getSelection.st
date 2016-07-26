model access
getSelection
	"Answer the model's selection interval."

	getSelectionSelector ifNil: [^1 to: 0].	"null selection"
	^model perform: getSelectionSelector withEnoughArguments: { self }