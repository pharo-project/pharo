private
makeQuantified: anRxmLink min: min max: max 
	"Perform recursive poor-man's transformation of the {<min>,<max>} quantifiers."
	| aMatcher |
	
	"<atom>{,<max>}       ==>  (<atom>{1,<max>})?"
	min = 0 ifTrue: [ 
		^ self makeOptional: (self makeQuantified: anRxmLink min: 1 max: max) ].
	
	"<atom>{<min>,}       ==>  <atom>{<min>-1, <min>-1}<atom>+"
	max ifNil: [
		^ (self makeQuantified: anRxmLink min: 1 max: min-1) pointTailTo: (self makePlus: anRxmLink copy) ].
	
	"<atom>{<max>,<max>}  ==>  <atom><atom> ... <atom>"
	min = max 
		ifTrue: [ 
			aMatcher := anRxmLink copy.
			(min-1) timesRepeat: [ aMatcher pointTailTo: anRxmLink copy ].
			^ aMatcher ].

	"<atom>{<min>,<max>}  ==>  <atom>{<min>,<min>}(<atom>{1,<max>-1})?"
	aMatcher := self makeOptional: anRxmLink copy.
	(max - min - 1) timesRepeat: [ 
		 aMatcher := self makeOptional: (anRxmLink copy pointTailTo: aMatcher) ].
	^ (self makeQuantified: anRxmLink min: min max: min) pointTailTo: aMatcher