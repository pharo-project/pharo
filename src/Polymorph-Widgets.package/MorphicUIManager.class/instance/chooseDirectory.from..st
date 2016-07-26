ui requests
chooseDirectory: label from: dir
	"Answer the user choice of a directory."
	
	| modalMorph realLabel |
	realLabel := label ifNil: ['Choose Directory' translated].
	(ProvideAnswerNotification signal: realLabel) ifNotNil: [:answer |
		^answer ].
	modalMorph := self modalMorph.
	^modalMorph theme  
		chooseDirectoryIn: modalMorph
		title: realLabel
		path: (dir ifNotNil: [dir fullName])