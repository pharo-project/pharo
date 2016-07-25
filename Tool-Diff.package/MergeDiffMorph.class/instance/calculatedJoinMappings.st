actions
calculatedJoinMappings
	"Specify click allowance for each section."
	
	^super calculatedJoinMappings do: [:j |
		j allowClick: self allowJoinClicks]