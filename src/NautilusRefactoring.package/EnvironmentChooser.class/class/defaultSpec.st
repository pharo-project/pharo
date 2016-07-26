specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newRow: [ :r| 
			r 
				add: #labelView width: 45; 
				add: #environmentsView ]