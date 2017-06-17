initialization
initializeWidgets
	usernameLabel := self newLabel label: 'Username: '.
	usernameInput := self newTextInput autoAccept: true.
	
	passwordLabel := self newLabel label: 'Password: '.
	passwordInput := self newTextInput beEncrypted; autoAccept: true.

	storeCheckbox := self newCheckBox
		label: 'Do you want this credentials to be stored for future use?';
		help: 'Please note that storing your credentials is handy but unsafe';
		state: true.
	
	self focusOrder
		add: usernameInput;
		add: passwordInput;
		add: storeCheckbox
