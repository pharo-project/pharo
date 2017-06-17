specs
defaultSpec 
	<spec>

	^ SpecLayout composed
		newColumn: [ :c |
			c 
				newRow: [:row | row add: #nameLabel width: 80; add: #namePanel ] height: 25;
				newRow: [:row | row add: #urlLabel width: 80; add: #urlPanel ] height: 25;
				add: #addButton height: self buttonHeight ];
		yourself