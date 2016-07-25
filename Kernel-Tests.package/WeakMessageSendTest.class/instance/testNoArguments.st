tests
testNoArguments
	"self run: #testNoArguments"

	| m |
	m := WeakMessageSend
			receiver: true
			selector: #yourself.
	self assert: (m value).
