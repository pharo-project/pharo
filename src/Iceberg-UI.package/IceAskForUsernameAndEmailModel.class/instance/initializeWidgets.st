initialization
initializeWidgets
	nameLabel := self newLabel label: 'user.name'.
	nameInput := self newTextInput 
		ghostText: 'John Doe';
		autoAccept: true.
	emailLabel := self newLabel label: 'user.email'.
	emailInput := self newTextInput 
		ghostText: 'johndoe@mail.net';		
		autoAccept: true.
	globalCheckbox := self newCheckBox
		label: 'Save properties as global?';
		state: false.
	acceptButton := self newButton
		label: 'Set properties';
		action: [ self accept ];
		yourself.	
			
	self focusOrder
		add: nameInput;
		add: emailInput;
		add: globalCheckbox;
		add: acceptButton