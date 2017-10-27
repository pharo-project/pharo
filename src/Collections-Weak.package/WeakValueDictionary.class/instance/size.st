accessing
size
	| count |
	count := 0.
	self valuesDo: [ :each | count := count + 1 ].
	^ count