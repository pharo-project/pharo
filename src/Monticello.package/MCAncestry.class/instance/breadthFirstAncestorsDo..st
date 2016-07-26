ancestry
breadthFirstAncestorsDo: aBlock
	| seen todo next |
	seen := Set with: self.
	todo := OrderedCollection with: self.
	[todo isEmpty] whileFalse:
		[next := todo removeFirst.
		next ancestors do:
			[:ea |
			(seen includes: ea) ifFalse:
				[aBlock value: ea.
				seen add: ea.
				todo add: ea]]]