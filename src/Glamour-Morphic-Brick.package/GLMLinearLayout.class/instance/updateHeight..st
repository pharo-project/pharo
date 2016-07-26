height
updateHeight: aBrick
	"this method should be called only when height of aBrick is dirty"
	aBrick isHeightClean ifTrue: [ Exception signal: 'Clean height can''t be updated!' ].
	aBrick layout vState updateHeight: aBrick on: self
	
	
	