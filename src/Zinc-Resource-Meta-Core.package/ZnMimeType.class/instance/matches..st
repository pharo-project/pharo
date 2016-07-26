testing
matches: aMimeType
	"Compare the receiver to aMimeType, which is the pattern to match, 
	it is a normal instance where main or sub can be wildcards.
	Matches should be used instead of #= to ignore parameters."
	
	^ (aMimeType main = '*'
		or: [ aMimeType main = self main ])
		and: [
			aMimeType sub = '*'
				or: [ aMimeType sub = self sub ] ]