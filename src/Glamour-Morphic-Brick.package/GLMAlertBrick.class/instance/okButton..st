accessing
okButton: anObject
	okButton := anObject.
	
	self okButton
		marginBottom: 3;
		text: 'OK';
		hAlign: #center;
		when: #onClicked do: [ self window ifNotNil: [ :w | w close ] ].
	
	self addBrickBack: self okButton