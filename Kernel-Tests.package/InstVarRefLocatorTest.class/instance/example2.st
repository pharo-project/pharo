examples
example2
	| ff|	
	ff := 1.
	(1 < 2) ifTrue: [ff ifNotNil: [ff := 'hallo']].
	^ ff.