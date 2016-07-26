private
childNodeFor: node
	"We find the child for a node, if it has not childs nil, if has more than one let the user select the one he wants."
	node ifNil:[^nil].
	^ node children isEmpty
		ifTrue: [ nil ]
		ifFalse: [ 
			node children size = 1
				ifTrue: [node children at: 1]
				ifFalse: [  self  askForNodeSelectionFrom: node children]
		 ]