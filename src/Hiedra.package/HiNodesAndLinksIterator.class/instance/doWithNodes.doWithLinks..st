enumerating
doWithNodes: nodesBlock doWithLinks: linksBlock 
	self prepareToIterate.
	
	linkBuilders do: [ :linkModel |
		values do: [ :each |
			self 
				doWithNodes: nodesBlock 
				doWithLinks: linksBlock
				from: each
				linkBuilder: linkModel ] ]