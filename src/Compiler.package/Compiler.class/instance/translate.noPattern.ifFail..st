private
translate: aStream noPattern: noPattern ifFail: failBlock
	^self parser
		parse: aStream
		class: class
		category: category
		noPattern: noPattern
		context: context
		notifying: requestor
		ifFail: [^failBlock value]