tests
testNoWeakBlock
	"There is no such thing as a weak block, these things never go away."

	<expectedFailure>

	| counter |
	counter := 0.

	(announcer subscribe: AnnouncementMockA do: [ :ann | counter := counter + 1 ]) makeWeak.
	
	Smalltalk garbageCollect.
	announcer announce: AnnouncementMockA.
	self assert: counter = 1.
	
	Smalltalk garbageCollect.
	announcer announce: AnnouncementMockA.
	self assert: counter = 2