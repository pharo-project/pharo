accessing
projectVersion
	| project workingCopy info |
	project := self project ifNil: [ ^ nil ].
	workingCopy := project workingCopy.
	info := workingCopy ancestors first.
	^ MCVersion  package: workingCopy info: info.
