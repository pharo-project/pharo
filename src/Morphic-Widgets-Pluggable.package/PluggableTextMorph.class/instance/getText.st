model access
getText
	"Retrieve the current model text"

	| newText |
	getTextSelector ifNil: [^Text new].
	newText := model perform: getTextSelector withEnoughArguments: {self}.
	newText ifNil: [^Text new].
	^newText shallowCopy