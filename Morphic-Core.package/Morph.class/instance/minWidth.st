layout
minWidth
	"answer the receiver's minWidth"
	^ self
		valueOfProperty: #minWidth
		ifAbsent: [2]