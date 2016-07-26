building
pointTailTo: aNode
	"See superclass for explanations."

	loopback
		ifTrue: [
			alternative == nil
				ifTrue: [alternative := aNode]
				ifFalse: [alternative pointTailTo: aNode]]
		ifFalse: [super pointTailTo: aNode]