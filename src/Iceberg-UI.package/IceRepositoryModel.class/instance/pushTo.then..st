private
pushTo: aRemote then: aBlock
	[ 
		UIManager default informUserDuring: [ :bar | 
			bar label: ('Publishing local commits to {1}' format: { aRemote url }).
			self repository pushTo: aRemote. 
			aBlock value ] ] 
	on: YouShouldPullBeforePush
	do: [ 
		UIManager default
			abort: 'Publish operation is not possible because your repository is out of date. 
Please update your local repository and retry.'
			title: 'Publishing aborted' ]