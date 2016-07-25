initialization
initializePresenter

	inputField whenTextChanged: [ :text | self updateRootFor: text ].
		
	tree whenTreeUpdated: [ inputField text: '' ].