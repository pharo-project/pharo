tests
testInvariant
	"The default invariant"
	"self run: #testInvariant"
	
	self assert: (self packageOrganizerClass default packageNames size > 50).
	"to be sure that this is real default one"
	self assert: (self announcer hasSubscriber: self packageOrganizerClass default).
	"note that this test is not precise enough because I could get the wrong one."
	Smalltalk garbageCollect.
	Smalltalk garbageCollect.
	self assert: self packageOrganizerClass allInstances size equals: 1.
	
	self packageOrganizerClass allInstances collect: [:each | self announcer hasSubscriber: each ] 
	"To debug..."