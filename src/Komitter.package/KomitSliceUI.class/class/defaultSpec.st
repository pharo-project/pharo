specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :col |
			col
				newRow: [ :row |
					row
						add: #issueNumber width: 90;
						add: #titleWidget ] height: self inputTextHeight;
				newRow: [ :row | ] ];
		yourself