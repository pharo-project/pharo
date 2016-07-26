initialize-release
initialize: aRegex ignoreCase: aBoolean 
	"Set `testMethod' variable to a can-match predicate block:
	two-argument block which accepts a lookahead character
	and a matcher (presumably built from aRegex) and answers 
	a boolean indicating whether a match could start at the given
	lookahead. "

	ignoreCase := aBoolean.
	prefixes := Set new: 10.
	nonPrefixes := Set new: 10.
	conditions := Set new: 3.
	methodPredicates := Set new: 3.
	nonMethodPredicates := Set new: 3.
	predicates := Set new: 3.
	nonPredicates := Set new: 3.
	lookarounds := Set new: 3.
	aRegex dispatchTo: self.	"If the whole expression is nullable, 
		end-of-line is an implicit can-match condition!"
	aRegex isNullable ifTrue: [conditions add: #atEndOfLine].
	testBlock := self determineTestMethod