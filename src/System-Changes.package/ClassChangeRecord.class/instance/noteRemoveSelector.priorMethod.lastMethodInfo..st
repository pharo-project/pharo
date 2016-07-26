method changes
noteRemoveSelector: selector priorMethod: priorMethod lastMethodInfo: infoOrNil

	| methodChange |
	methodChange := self findOrMakeMethodChangeAt: selector priorMethod: priorMethod.
	methodChange changeType == #add
		ifTrue: [methodChange noteChangeType: #addedThenRemoved]
		ifFalse: [methodChange noteChangeType: #remove].

	infoOrNil ifNotNil:
		["Save the source code pointer and category so can still browse old versions"
		methodChange noteMethodInfoFromRemoval: infoOrNil]

