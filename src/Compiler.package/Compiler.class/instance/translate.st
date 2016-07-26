public access
translate
	"once we will have change all the sender of MethodNode>>#generate: to use compile, we can remove the methodNode encoder requestor: requestor."
	| methodNode |
	methodNode := self parser
		parse: sourceStream
		class: class
		category: category
		noPattern: self compilationContext noPattern
		context: context
		notifying: requestor
		ifFail: [^ self compilationContext failBlock value].
	methodNode encoder requestor: requestor.
	^ methodNode