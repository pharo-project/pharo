comparing
= aMessageTally

	self species == aMessageTally species ifFalse: [^ false].
	^ aMessageTally method == method 
		and: [ aMessageTally process == process ]