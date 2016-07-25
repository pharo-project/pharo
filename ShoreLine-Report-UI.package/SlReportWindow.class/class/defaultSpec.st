specs
defaultSpec
	<spec>

	^ SpecLayout composed
		newColumn: [ :column |
		column
			newRow: [ :row | row
				add: #sendButton;
				add: #cancel ] height: 26;
			newRow: [ :row | row
				add: #autoSubmit;
				add: #help width: 16 ] height: 20;
			newRow: #(stack) ]