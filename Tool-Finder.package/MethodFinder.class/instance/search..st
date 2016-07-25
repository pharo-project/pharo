public api
search: multi
	"if Multi is true, collect all selectors that work."
	selector := OrderedCollection new.	"list of them"
	self simpleSearch.
	multi not & (selector isEmpty not) ifTrue: [^ selector].

	[self permuteArgs] whileTrue:
		[self simpleSearch.
		multi not & (selector isEmpty not) ifTrue: [^ selector]].
	self insertConstants.
	^ #()