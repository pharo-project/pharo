Extension { #name : 'String' }

{ #category : '*Deprecated15-Kernel' }
String >> endsWithDigit [
	"Answer whether the receiver's final character represents a digit."
	"'foo10' endsWithDigit >>> true"
	"'foo10foo' endsWithDigit >>> false"
	"'foo1' endsWithDigit >>> true"

	^ self notEmpty and: [self last isDigit]
]

{ #category : '*Deprecated15-Kernel' }
String >> numericSuffix [
	"'abc98' numericSuffix >>> 98"
	"'98abc' numericSuffix >>> 0"
	
	^ self stemAndNumericSuffix last
]

{ #category : '*Deprecated15-Kernel' }
String >> stemAndNumericSuffix [
	"Parse the receiver into a string-valued stem and a numeric-valued suffix."
	
	"'Fred2305' stemAndNumericSuffix >>> #('Fred' 2305)"

	| stem suffix position |
	stem := self.
	suffix := 0.
	position := 1.
	[stem endsWithDigit and: [stem size > 1]] whileTrue:
		[suffix :=  stem last digitValue * position + suffix.
		position := position * 10.
		stem := stem copyFrom: 1 to: stem size - 1].
	^ { stem . suffix }
]
