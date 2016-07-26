accessing
rootsFrom: aCollectionOfNodes
	"Answer the receiver's roots"
	^ aCollectionOfNodes
		select: [:each | each indentLevel isZero]