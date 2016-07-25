private
initPattern: aString notifying: req return: aBlock

	| result |
	self
		init: aString asString readStream
		notifying: req
		failBlock: [^nil].
	encoder := self.
	result := aBlock value: (self pattern: false inContext: nil).
	encoder := failBlock := nil.  "break cycles"
	^result