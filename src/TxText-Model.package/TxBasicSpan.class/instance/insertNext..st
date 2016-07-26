accessing
insertNext: aSpans
	"insert new span(s) after receiver. "
	| oldNext |
	oldNext := next.
	aSpans prev: self.
	
	(oldNext == self)
		ifFalse: [ aSpans last next: oldNext ].

	^ aSpans