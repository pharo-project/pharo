comparing
caseSensitiveLessOrEqual: aString 
	"Answer whether the receiver sorts before or equal to aString.
	The collation order is case sensitive."
	^(self compare: aString caseSensitive: true) <= 2