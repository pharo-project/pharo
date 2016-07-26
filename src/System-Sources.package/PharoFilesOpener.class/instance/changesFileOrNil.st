public
changesFileOrNil
	| changes |
	changes := self openChanges: self changesName.
	changes
		ifNil: [ 
			self informProblemInChanges: self cannotLocateMsg.
			^ nil ].
	(changes isReadOnly and: [ self shouldInformAboutReadOnlyChanges ])
		ifTrue: [ self informProblemInChanges: self cannotWriteMsg ].
	((changes next: 200) includesSubstring: String crlf)
		ifTrue: [ self informProblemInChanges: self badContentMsg ].
	^ changes