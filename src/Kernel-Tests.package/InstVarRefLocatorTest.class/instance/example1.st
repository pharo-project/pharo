examples
example1
	| ff |
	(1 < 2) ifTrue: [tt ifNotNil: [ff := 'hallo']].
	^ ff.