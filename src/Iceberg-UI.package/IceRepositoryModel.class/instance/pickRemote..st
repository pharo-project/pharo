private
pickRemote: aTitle
	| remotes |
	 
	remotes := self repository remotes.
	^ UIManager default
		chooseFrom: (remotes collect: #remoteName)
		values: remotes
		message: 'Select the remote you want to use:' 
		title: aTitle