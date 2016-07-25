accessing
contentFromModel
	"Answer content from the model."
	
	^self getContentSelector ifNotNil: [:s |
		self model ifNotNil: [:m | m perform: s]]