specs
defaultSpec 
	<spec>
	^ SpecLayout composed
		newRow: [ :row | 
			row 
				add: #locationLabel width: 100; 
			 	add: #locationInput; 
				add: #chooseButton width: self iconWidth ]