accessing
childrenItems
	"I search the children, if I have not got any, I call my super method"

	self model isPragmasSymbol
		ifTrue: [ ^  (self model resultDictionary at: parentNode item ifAbsent:[^super childrenItems]) at: self item ifAbsent: [ ^ super childrenItems]].
	^ (self model resultDictionary at: self item ifAbsent:[^super childrenItems])