morphic ui
buttonSpecs
	^ #(('instance' switchBeInstance 'show instance' buttonEnabled switchIsInstance)
		('?' switchBeComment 'show comment' buttonEnabled switchIsComment)
		('class' switchBeClass 'show class' buttonEnabled switchIsClass))