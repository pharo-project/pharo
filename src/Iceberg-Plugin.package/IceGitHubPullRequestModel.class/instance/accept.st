actions
accept
	| title |
	
	title := self titlePanel text trim.
	title ifEmpty: [ 
		UIManager default alert: 'You need to specify a title for the pull request.' title: 'Error'. 
		^ self ].
	
	acceptBlock ifNotNil: [ 
		acceptBlock cull: self createPullRequest ].
	
	self window delete