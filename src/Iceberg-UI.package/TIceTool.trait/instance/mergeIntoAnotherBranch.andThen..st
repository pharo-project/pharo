utilities
mergeIntoAnotherBranch: aCommit andThen: aBlock
	"Merge selected commit into another branch"
	| branches selection repository |
	repository := aCommit repository.
	
	repository isModified ifTrue: [ 
		(self uiManager
			confirm:
'Your repository has uncommited changes. Merge will change your current 
working copy and your current state will not be recoverable. 
We recommend that you commit first and then pull incoming changes again.'
			label: 'You might loose your current changes!'
			trueChoice: 'Proceed anyway'
			falseChoice: 'Cancel'
			cancelChoice: nil
			default: false) 
			ifFalse: [ ^ self ].
	 ].	
	branches := repository branchesForMerge: aCommit.
	selection := self uiManager
		chooseFrom: (branches collect: #name)
		values: branches
		message: ('Select the branch into you want to merge commit {1}' format: { aCommit shortId })
		title: 'Merge your changes into another branch'.

	selection ifNotNil: [ 
		[ aCommit mergeInto: selection ]
			on: IceMergeAborted
			do: [ 
				selection name = repository branchName 
					ifTrue: [ repository mergeConflictsWith: aCommit ]
					ifFalse: [ 
						self uiManager 
						abort: 'Automatic (fast-forward) merge is not possible, 
to merge manually please first checkout 
branch ', selection name, ' and then retry.' 
						title: 'Automatic merge failed' ]
			].

		aBlock value 
	]
