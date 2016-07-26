actions
merge: aSpan
	self characters: ((self characters), aSpan characters).
	"I'am at the end of the text"
	(self next  = self next next) ifTrue:[
			self next: self.
		]
	ifFalse:[
			self next: aSpan next.
		].
	
	
	