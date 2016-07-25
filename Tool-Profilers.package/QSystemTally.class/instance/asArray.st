converting
asArray
	| link |
	^Array streamContents: [ :s |
		link := self.
		[link == nil] whileFalse: [
			s nextPut: link.
			link := link nextLink.
		].
	].