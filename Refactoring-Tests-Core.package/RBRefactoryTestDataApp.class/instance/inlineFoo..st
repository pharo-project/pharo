test
inlineFoo: aBlock 
	| bar baz asdf |
	bar := aBlock value: self.
	baz := bar + bar.
	asdf := baz + bar.
	^asdf