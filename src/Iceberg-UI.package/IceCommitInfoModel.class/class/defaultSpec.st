specs
defaultSpec 
	<spec>

	^ SpecLayout composed
		newColumn: [ :c |
			c 
				newRow: [:row | row add: #dateLabel width: 80; add: #datePanel ] height: 25;
				newRow: [:row | row add: #idLabel width: 80; add: #idPanel ] height: 25;
				newRow: [:row | row add: #parentsLabel width: 80; add: #parentsPanel ] height: 25;
				newRow: [:row | row add: #authorLabel width: 80; add: #authorPanel ] height: 25;
				newRow: [:row | row add: #commentLabel width: 80; add: #commentPanel ] ];
		yourself