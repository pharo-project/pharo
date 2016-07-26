converting
stemAndNumericSuffix
	"Parse the receiver into a string-valued stem and a numeric-valued suffix."
	"'Fred2305' stemAndNumericSuffix"

	| stem suffix position |

	stem := self.
	suffix := 0.
	position := 1.
	[stem endsWithDigit and: [stem size > 1]] whileTrue:
		[suffix :=  stem last digitValue * position + suffix.
		position := position * 10.
		stem := stem copyFrom: 1 to: stem size - 1].
	^ Array with: stem with: suffix
