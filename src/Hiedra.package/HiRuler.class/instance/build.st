building
build
	freeColumnIndexByRow := Array new: values size withAll: 1. 
	nodes := OrderedCollection new.
	links := OrderedCollection new.
	
	HiNodesAndLinksIterator new 
		linkBuilders: linkBuilders;
		values: values;
		doWithNodes: [ :node | self processNode: node ]
		doWithLinks: [ :link | self processLink: link ]