private
waitWhile: aBlock inQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil
	[aBlock value] whileTrue: [self exitAndWaitInQueue: anOrderedCollection maxMilliseconds: anIntegerOrNil].