testing
canPush
	[ 
		^ self isOperative 
			and: [ self outgoingCommits notEmpty 
			and: [ self incomingCommits isEmpty ] ]
	]
	on: Error do: [ ^ false ]