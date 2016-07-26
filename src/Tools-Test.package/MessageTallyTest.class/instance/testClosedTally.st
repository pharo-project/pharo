tallying
testClosedTally

	| tally |
	tally := MessageTally
				tallySendsTo: nil 
				inBlock:  [ 3.14159 printString ] 
				showTree: true 
				closeAfter: true 
				openResultWindow: false.
				
	"The tally is closed, therefore nothing may be accessed"
	self assert: (tally receivers isNil)