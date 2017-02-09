private
isYellowButtonReallyPressed: anEvent
	anEvent yellowButtonPressed ifFalse: [ ^false ].
	"this is shitty fix for VM bug. 
	Now if you will press left mouse button together with pressed cmd (on Mac)
	then you will got right mouse button event.
	Interesting that it is not a problem in case of external SDL2 window.
	Try check it from OSWindowWorldMorph new open"
	^ (Smalltalk os isMacOS) 
		ifTrue: [ anEvent commandKeyPressed not]
		ifFalse: [ anEvent controlKeyPressed not]