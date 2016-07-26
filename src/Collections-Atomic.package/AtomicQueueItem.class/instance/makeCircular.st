accessing
makeCircular
	"Make a receiver circular, i.e. point to itself,
	answer the old value of next variable. 
	Note, this operation should be atomic"
	
	| temp |

	" atomic swap here"
	temp := next.
	next := self.

	^ temp