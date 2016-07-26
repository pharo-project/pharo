private
format: aStream noPattern: noPattern ifFail: failBlock
	^(self parser
		parse: aStream
		class: class
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]) preen