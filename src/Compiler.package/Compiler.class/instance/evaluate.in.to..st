public - old
evaluate: aString in: aContext to: aReceiver
	"evaluate aString in the given context, and return the result."
	| result |
	result := self
				evaluate: aString
				in: aContext
				to: aReceiver
				notifying: nil
				ifFail: [^ #failedDoit].
	^ result