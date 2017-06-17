specs
defaultSpec
	<spec: #default>

	^ SpecLayout composed newColumn: [ :col |
		col 
			newRow: [:row | row add: #usernameLabel width: 100; add: #usernameInput ] height: self inputTextHeight;
			newRow: #publicKeyLocation height: self inputTextHeight;
			newRow: #privateKeyLocation  height: self inputTextHeight;
			newRow: [:row | row add: #passwordLabel width: 100; add: #passwordInput ] height: self inputTextHeight;
			add: #storeCheckbox height: self inputTextHeight ]
