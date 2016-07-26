private - updating
updateDiff: selectedNode
	selectedNode 
		ifNil: [ 	self diff contextClass: nil; leftText: ''; rightText: '' ]
		ifNotNil: [selectedNode content updateDiff: self diff ]
