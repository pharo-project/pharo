public - old
compileNoPattern: textOrStream in: aClass context: aContext notifying: aRequestor ifFail: failBlock
	"Similar to #compile:in:notifying:ifFail:, but the compiled code is
	expected to be a do-it expression, with no message pattern."

	self from: textOrStream
		class: aClass
		context: aContext
		notifying: aRequestor.
	^self
		translate: sourceStream
		noPattern: true
		ifFail: failBlock