private
commandOrCrontrolKeyPressed: anEvent
	^ (Smalltalk os isMacOS) 
		ifTrue: [ anEvent controlKeyPressed ]
		ifFalse: [ anEvent commandKeyPressed ]