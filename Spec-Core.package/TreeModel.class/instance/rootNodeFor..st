api
rootNodeFor: item
	| node |
	
	node := self rootNodeHolder cull: item.
	node container: self.
	
	^ node