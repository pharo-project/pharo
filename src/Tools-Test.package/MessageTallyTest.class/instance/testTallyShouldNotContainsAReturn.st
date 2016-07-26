tallying
testTallyShouldNotContainsAReturn

	"The block passed to MessageTally should not have a ^ "
	
	self should: [ MessageTally
					tallySendsTo: nil 
					inBlock:  [ ^ 3.14159 printString ] 
					showTree: true 
					closeAfter: false 
					openResultWindow: false ] raise: Error.
	