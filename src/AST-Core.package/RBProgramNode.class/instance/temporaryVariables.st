accessing
temporaryVariables
	^parent ifNil: [#()] ifNotNil: [parent temporaryVariables]