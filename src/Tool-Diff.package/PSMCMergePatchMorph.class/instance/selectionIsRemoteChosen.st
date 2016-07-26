as yet unclassified
selectionIsRemoteChosen
	self selectedChange ifNil: [ ^ false].
	^ self selectedChange remoteChosen