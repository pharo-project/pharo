conversion
needsConversion
	"Answer whether conversion is required for the receiver's object class."

	^self objectClass notNil and: [(self objectClass includesBehavior: String) not]