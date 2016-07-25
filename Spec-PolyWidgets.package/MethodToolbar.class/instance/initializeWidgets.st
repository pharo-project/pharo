initialization
initializeWidgets
	browseModel := self newButton.
	usersModel := self newButton.
	sendersModel := self newButton.
	implementorsModel := self newButton.
	versionModel := self newButton.
	dropListModel := self newDropList.
	self
		setFocusOrder;
		setBrowseModel;
		setUsersModel;
		setVersionModel;
		setSendersModel;
		setImplementorsModel