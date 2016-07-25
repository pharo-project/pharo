utility
selectRepositoryFromWorkingCopy: workingCopy
	| repos repo |
	repos := workingCopy repositoryGroup repositories.
	repos size = 0
		ifTrue: [ ^ self uiManager  inform: 'No repositories associated with ' , target name printString ].
	repo := repos size = 1
		ifTrue: [ repos first ]
		ifFalse: [ 
			self uiManager 
				chooseFrom: (repos collect: [ :r | r description ])
				values: repos
				title: 'Please choose which repository to use for diff' ].
	^ repo