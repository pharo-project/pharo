private
addEmptyElementTo: result
	result isEmpty ifTrue: [result add: '']
		