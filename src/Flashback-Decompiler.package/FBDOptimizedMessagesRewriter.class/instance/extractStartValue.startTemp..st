private
extractStartValue: msgNode startTemp: startTemp
	| assignement indexOfMsg |
	indexOfMsg := msgNode parent statements indexOf: msgNode.
	indexOfMsg = 1
		ifTrue: [ ^ nil ].
	assignement := msgNode parent statements at: indexOfMsg - 1.
	(assignement isAssignment and: [ assignement variable = startTemp])
		ifFalse: [ ^ nil ].
	^ assignement value