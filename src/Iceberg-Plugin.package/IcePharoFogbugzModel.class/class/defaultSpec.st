specs
defaultSpec
	<spec>
	
	^ SpecLayout composed
		newRow: [ :row |
			row 
				add: #issueNumberText width: 50;
				add: #issueText;
				add: #createButton width: 80 ] height: self inputTextHeight