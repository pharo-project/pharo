private
toBraceStack: itsSize 
	"Push receiver's elements onto the stack of thisContext sender.  Error if receiver does
	 not have itsSize elements or if receiver is unordered.
	 Do not call directly: this is called by {a. b} := ... constructs."

	self size ~= itsSize ifTrue:
		[self error: 'Trying to store ', self size printString,
					' values into ', itsSize printString, ' variables.'].
	thisContext sender push: itsSize fromIndexable: self