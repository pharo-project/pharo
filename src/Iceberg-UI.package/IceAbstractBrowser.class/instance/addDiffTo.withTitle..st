building
addDiffTo: browser withTitle: diffTitle
	browser transmit 
		from: #changes; 
		to: #diff; 
		andShow: [ :a | 
			a swappedColorDiff
				title: diffTitle;
				display: [:change | self buildDiffInput: change ]]
