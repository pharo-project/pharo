public
gtExamplesContained
	" self gtExamplesContained "
	| examples |
	examples := OrderedCollection new.
	self gtExamplesContainedTo: examples.
	^ examples