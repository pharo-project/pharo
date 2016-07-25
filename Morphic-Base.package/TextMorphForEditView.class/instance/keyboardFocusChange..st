changed
keyboardFocusChange: gotFocus 
	self announceKeyboardFocusChange: gotFocus.
 	paragraph 
		ifNotNil: [paragraph focused: gotFocus].
 	gotFocus 
 		ifTrue: [
			"A hand is wanting to send us characters..."
 			self hasFocus 
				ifFalse: [self editor "Forces editor install"].
			self editor focused: true.
			self showOverEditableTextCursor.
			self useFindReplaceSelection
				ifTrue: [self selectionChanged]] 
		ifFalse:[editor ifNotNil: [editor focused: false].
			self hideOverEditableTextCursor].
	self manageCursor.		
 	self editView
		ifNil: [self focusChanged]
		ifNotNil: [self editView focusChanged].
	self acceptOnFocusChange == true
		ifTrue: [self editView hasUnacceptedEdits ifTrue: [self editor accept]]