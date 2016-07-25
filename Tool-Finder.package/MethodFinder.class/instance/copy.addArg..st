initialize
copy: mthFinder addArg: aConstant
	"Copy inputs and answers, add an additional data argument to the inputs.  The same constant for every example"
	| more |
	more := Array with: aConstant.
	data := mthFinder data collect: [:argList | argList, more].
	answers := mthFinder answers.
	self load: nil.
