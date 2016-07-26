accessing
size
	"answer 1 + the number of segments next to receiver"
	
	| segment count |
	segment := self.
	count := 0.
	[ segment notNil ] whileTrue: [  count := count + 1. segment := segment next ].

	^ count.