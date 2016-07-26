user-interface
dialogIsActiveFor: aTextArea
	^ self dialog world notNil and: [ self textArea = aTextArea  ]