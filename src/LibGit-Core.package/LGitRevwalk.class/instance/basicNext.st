private
basicNext
	"Note: in the C code you would check the return code to see when the loop is over.

	This method uses instances variables to enable more Smalltalk like looping:
	[ walk hasNext ] whileTrue: [ walk next ]."
	| previousCommit nextId |
	previousCommit := currentCommit.
	nextId := self basicNextObjectId.
	currentCommit := self hasNext
		ifTrue: [
			LGitCommit
				of: self repository
				fromId: nextId ]
		ifFalse: [ nil ].
	
	^ previousCommit