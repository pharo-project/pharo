accessing
isOpaque
	"Return true if the receiver is marked as being completely opaque"
	
	^ self
		valueOfProperty: #isOpaque
		ifAbsent: [false]