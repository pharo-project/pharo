accessing
treeHolder
	^ treeHolder
		ifNil: [treeHolder := self newTreeHolder whenChangedSend: #treeChanged: to: self; yourself]