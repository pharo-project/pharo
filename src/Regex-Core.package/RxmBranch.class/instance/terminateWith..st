building
terminateWith: aNode
	"See superclass for explanations."

	loopback
		ifTrue: [alternative == nil
			ifTrue: [alternative := aNode]
			ifFalse: [alternative terminateWith: aNode]]
		ifFalse: [super terminateWith: aNode]