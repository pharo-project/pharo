dropping/grabbing
dropEnabled
	"Get this morph's ability to add and remove morphs via drag-n-drop."

	^ (self valueOfProperty: #dropEnabled) ~~ false
