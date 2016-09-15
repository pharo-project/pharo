private
processNode: node
	
	nodes add: node.
	node columnIndex: (freeColumnIndexByRow at: node rowIndex).
	self indentAt: node rowIndex.
