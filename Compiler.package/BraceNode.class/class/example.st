examples
example
	"Test the {a. b. c} syntax."

	| x |
	x := {1. {2. 3}. 4}.
	^ {x first. x second first. x second last. x last. 5} as: Set

"BraceNode example Set (0 1 2 3 4 5 )"
