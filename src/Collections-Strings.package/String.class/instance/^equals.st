comparing
= aString 
	"Answer whether the receiver sorts equally as aString.
	The collation order is simple ascii (with case differences)."
	
	(aString isString and: [self size = aString size]) ifFalse: [^false].
	^ (self compare: self with: aString collated: AsciiOrder) = 2