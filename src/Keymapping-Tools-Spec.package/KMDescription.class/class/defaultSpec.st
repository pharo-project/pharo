specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newColumn: [:c | 
			c 
				newRow: [:r | 
					r 
						add: #categoryList;
						add: #orderingList ] height: 25;
				add: #shortcutList;
				add: #okToolbar height: 25 ]