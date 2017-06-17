actions
switchBranchTo: branch
	branch isRemote
		ifTrue: [ self repository localBranches 
				detect: [:aBranch | aBranch name = branch basename ]
				ifFound: [ UIManager default inform: 'Cannot switch to this branch. A local branch with the same name already exists!'.
								self flag: 'We should probably go into detached head state.' ]
				ifNone: [ self shouldCreateLocalBranchFromRemote 
					ifTrue: [ self repository backend withRepoDo: [ :repo | repo createBranchFromOrigin: branch basename ].
								self repository checkoutBranch: branch basename ]
					ifFalse: [ self shouldBeImplemented ] ] ]
		ifFalse: [ self repository checkoutBranch: branch basename ]