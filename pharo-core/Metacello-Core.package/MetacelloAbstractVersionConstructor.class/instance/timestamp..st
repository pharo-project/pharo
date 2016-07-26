api
timestamp: aBlockOrStringOrDateAndTime
    "Define timestamp field of version spec (MetacelloMCVersionSpec).
	 If <aBlockOrStringOrDateAndTime> is a String, the version spec timetamp is set to the String.
	 If <aBlockOrStringOrDateAndTime> is a DateAndTime, the version spec timetamp is set to the printString of the DateAndTime.
	If <aBlockOrStringOrDateAndTime> is a Block, the specifications in <aBlockOrStringOrDateAndTime> are applied to the timestamp spec (MetacelloValueHolderSpec). Not Recommended!
	
		spec timestamp: '10/7/2009 14:40'.
		
		spec timestamp: DateAndTime now'.
		
		spec timestamp: [
			spec value: '10/7/2009 14:40'. ].
    "

    self root timestamp: aBlockOrStringOrDateAndTime constructor: self