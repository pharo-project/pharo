private
cut: aContext
	"Cut aContext and its senders from my sender chain"

	| context callee |
	context := self.
	[ context == aContext ] whileFalse: [
		callee := context.
		context := context sender.
		context ifNil: [
			aContext ifNotNil: [
				self error: 'aContext not a sender' ]]].
	callee privSender: nil.
