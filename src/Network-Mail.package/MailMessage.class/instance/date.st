accessing
date
	"Answer a date string for this message."

	^(Date fromSeconds: self time + (Date year: 1980 day: 1 ) asSeconds) 
		printFormat: #(2 1 3 47 1 2)