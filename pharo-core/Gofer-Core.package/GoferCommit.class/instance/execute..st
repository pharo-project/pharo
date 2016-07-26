running
execute: aWorkingCopy
	| repositories version |
	repositories := self gofer repositories
		reject: [ :repository | (aWorkingCopy changesRelativeToRepository: repository) isEmpty ].
	repositories isEmpty
		ifTrue: [ ^ self ].
	version := [ aWorkingCopy newVersionIn: (MCRepositoryGroup withRepositories: repositories) ]
		on: MCVersionNameAndMessageRequest
		do: [ :notifcation |
			self message ifNil: [ message := notifcation outer last ].
			notifcation resume: (Array with: notifcation suggestedName with: self message) ].
	self gofer repositories
		do: [ :repository | repository storeVersion: version ]