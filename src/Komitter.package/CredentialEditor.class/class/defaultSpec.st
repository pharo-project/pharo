specs
defaultSpec
	<spec: #default>
	
	^ SpecLayout composed
		newColumn: [ :col |
			col 
				newRow: [ :r | r add: #usernameLabel width: 80; add: #username ] height: self inputTextHeight;
				newRow: [ :r | r add: #passwordLabel width: 80; add: #password ] height: self inputTextHeight + 4;
				newRow: [ :r | r add: #showPasswordWidget width: 120; newColumn: [ :c | ] ] height: self inputTextHeight + 4;
				newRow: [ :r | ]
		];
	yourself