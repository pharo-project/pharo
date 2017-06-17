utilities
pushRepository: repository andThen: aBlock
	[ UIManager default informUserDuring: [ :bar | | remote |
		remote := repository pushRemote.
		bar label: ('Publishing local commits to {1}' format: {remote}).
		repository pushTo: remote. 
		aBlock value.
	]] 
	on: YouShouldPullBeforePush
	do: [ 
		self uiManager
			abort: 'Publish operation is not possible because your repository is out of date. 
Please update your local repository and retry.'
			title: 'Publishing aborted' 
	]
