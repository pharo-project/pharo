*Slot
temporaryVariables
	^self tempNames collect: [ :name | TemporaryVariable new name: name ]
