building
addDiffTo: browser withTitle: diffTitle
	browser transmit 
		from: #changes; 
		to: #diff; 
		andShow: [ :a | 
			a diff 
				title: diffTitle;
				display: [:change | { 
					change myVersion ifNil: ''. 
					change theirVersion ifNil: '' } ]]
