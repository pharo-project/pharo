private protocol
lookForCode: code ifDifferent: handleBlock 
	"We are expecting a certain numeric code next.  
	However, in the FTP protocol, multiple lines are allowed.  
	If the response is multi-line, the fourth character of the first line is a  
	$- and the last line repeats the numeric code but the code is followed by 
	a space. So it's possible that there are more lines left of the last response that 
	we need to throw away. We use peekForAll: so that we don't discard the
	next response that is not a continuation line."

	
	"check for multi-line response"
	(self lastResponse size > 3
			and: [(self lastResponse at: 4) = $-])
		ifTrue: ["Discard continuation lines."
			 | headToDiscard |
			headToDiscard := self lastResponse first: 4.
			[[self stream peekForAll: headToDiscard]
				whileTrue: [self stream nextLine]]
				on: Exception
				do: [:ex | ^handleBlock value: nil]].
	^ super lookForCode: code ifDifferent: handleBlock