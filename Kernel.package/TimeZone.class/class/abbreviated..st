accessing
abbreviated: aString
 	"Return the timezone whose abbreviation is aString."
 	^ self timeZones detect: [ :timeZone | 
		timeZone abbreviation = aString ].