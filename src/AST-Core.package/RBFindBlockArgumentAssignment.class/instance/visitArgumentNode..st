visiting
visitArgumentNode: anArgumentNode
	"register argument node if it is an block argument"

	anArgumentNode parent isBlock
		ifTrue: [ arguments add: anArgumentNode ]