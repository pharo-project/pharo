private
pickGroupsOnCancel: aBlock
	| result |
	result := UIManager default 
		request: 'Groups to install (a comma separated string)' 
		initialAnswer: '' 
		title: 'Enter groups to install'.
	result ifNil: [ ^ aBlock value ].
	^ result