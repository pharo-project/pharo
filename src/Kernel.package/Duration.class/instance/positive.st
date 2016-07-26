accessing
positive
 
 
 	^ seconds = 0 ifTrue: [ nanos positive ] ifFalse: [ seconds positive ]
 