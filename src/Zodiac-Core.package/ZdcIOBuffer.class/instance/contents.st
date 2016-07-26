operations
contents
	"Return our readable contents"
	
	^ self isEmpty
		ifTrue: [ buffer copyEmpty ]
		ifFalse: [ buffer copyFrom: self contentsStart to: self contentsEnd ]