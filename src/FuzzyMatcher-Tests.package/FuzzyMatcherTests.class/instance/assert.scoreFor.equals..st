asserting
assert: matcher scoreFor: aString equals: aBlockOrNumber

	matcher match: aString ifScored: [ :score |
		self assert: score equals: aBlockOrNumber value.
		^ self	
	].

	self error: 'Expected to match: ', aString