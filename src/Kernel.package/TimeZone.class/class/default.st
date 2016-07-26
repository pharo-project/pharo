accessing
default
 	"Answer the default time zone - GMT"
 
 	^ self timeZones detect: [ :timeZone |
		timeZone offset = Duration zero ]
 