fields
fieldNamed: aString ifAbsent: aBlock
	| matchingFields |
	"return the value of the field with the specified name.  If there is more than one field, then return the first one"
	matchingFields := fields at: aString asLowercase ifAbsent: [ ^aBlock value ].
	^matchingFields first