platform independent access
floatAt: index bigEndian: boolean 
	^ Float
		fromIEEE32Bit: (self unsignedLongAt: index bigEndian: boolean)