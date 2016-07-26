accessing
okToDiscardEdits 
	^ self canDiscardEdits  or: [self promptForCancel]