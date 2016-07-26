accessing
selectedClass
	^ self selectedNode 
		ifNotNil: [ :currNode | currNode methodClass ]