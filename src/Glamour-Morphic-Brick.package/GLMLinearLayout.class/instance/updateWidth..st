width
updateWidth: aBrick
	"aBrick is a Brick that uses me as current layout"
	"this method should be called only when width of aBrick is dirty"
	aBrick isWidthClean ifTrue: [ Exception signal: 'Clean width can''t be updated!' ].
	aBrick layout hState updateWidth: aBrick on: self
	
	
	