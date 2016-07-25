private
readOneHeaderFrom: stream
	| key value |
	key := stream upTo: $:.
	[ stream peek == Character space ] whileTrue: [ stream next ].
	value := stream upToEnd.
	self at: key add: value.
	^ key