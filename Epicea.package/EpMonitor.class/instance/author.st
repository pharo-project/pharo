private
author
	^ Author uniqueInstance 
		ifUnknownAuthorUse: 'Unknown' 
		during: [ Author uniqueInstance fullName ]
