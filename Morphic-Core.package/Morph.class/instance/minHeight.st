layout
minHeight
	"answer the receiver's minHeight"
	^ self
		valueOfProperty: #minHeight
		ifAbsent: [2]