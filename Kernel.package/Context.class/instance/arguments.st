accessing
arguments
	"returns the arguments of a message invocation"
	
	| arguments numargs |
	numargs :=  self method numArgs.
	arguments := Array new: numargs.
	1 to: numargs do: [ :i | arguments at: i put: (self tempAt: i) ].
	^ arguments