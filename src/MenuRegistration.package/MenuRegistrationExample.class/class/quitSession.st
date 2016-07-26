menu actions
quitSession
	Smalltalk
		snapshot: (UIManager default confirm: 'Save changes before quitting?' translated orCancel: [ ^ self ])
		andQuit: true