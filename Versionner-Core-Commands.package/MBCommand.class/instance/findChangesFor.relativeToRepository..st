changes
findChangesFor: workingCopy relativeToRepository: repository
	| patch fromDescription |
	repository isNil
		ifTrue: [ ^ self ].
	patch := workingCopy changesRelativeToRepository: repository.
	patch isNil
		ifTrue: [ ^ self ].
	patch isEmpty
		ifTrue: [ 
			workingCopy modified: false.
			self uiManager inform: 'No changes' ]
		ifFalse: [ 
			workingCopy modified: true.
			fromDescription := workingCopy packageName , ' (' , workingCopy ancestry ancestorString , ')'.
			self
				viewChanges: patch
				from: fromDescription
				to: ('Modified {1}' translated format: {(workingCopy description)}) 
				title: ('Changes to {1}' translated format: {fromDescription}) ]