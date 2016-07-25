accessing
textArea: aTextArea
	textAreaHolder at: 1 put: aTextArea.
	aTextArea announcer 
		when: RubFindReplaceWindowRequired send: #whenFindReplaceWindowRequired: to: self. 
