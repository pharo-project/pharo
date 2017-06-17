accessing
tree
	<todo: 'use proper exceptions'>
	^ tree ifNil: [ self error: 'need an existing tree here' ]