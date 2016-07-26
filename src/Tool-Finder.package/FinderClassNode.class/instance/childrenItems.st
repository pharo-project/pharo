accessing
childrenItems
	"I search the children, if I have not got any, I call my super method"
	^ (self model resultDictionary at: self item ifAbsent:[^super childrenItems])