halos and balloon help
editBalloonHelpContent: aString
	| reply |
	reply := UIManager default
		multiLineRequest: 'Edit the balloon help text for ' translated, self externalName
		initialAnswer: (aString ifNil: [self noHelpString] ifNotNil: [aString])
		answerHeight: 200.
	reply ifNil: [^ self].  "User cancelled out of the dialog"
	(reply isEmpty or: [reply asString = self noHelpString])
		ifTrue: [self setBalloonText: nil]
		ifFalse: [self setBalloonText: reply]