accessing
nextCount
	"Global counter to avoid name clash between test runs, in case of some previous failure."
	^Counter := (Counter ifNil: [ 0 ]) + 1.