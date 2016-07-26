action
delete
	| m |
	m := model.
	[ super delete ]
		ensure: [ 
			model := m.
			model textArea ifNotNil: [ :ta | ta takeKeyboardFocus ] ]