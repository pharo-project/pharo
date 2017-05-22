specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col 
			newRow: [:row | row add: #nameLabel width: 100; add: #nameInput ] height: self inputTextHeight;
			newRow: [:row | row add: #emailLabel width: 100; add: #emailInput ] height: self inputTextHeight;
			add: #globalCheckbox height: self inputTextHeight;
			add: #acceptButton height: self buttonHeight  ]
