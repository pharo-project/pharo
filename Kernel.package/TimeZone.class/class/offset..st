accessing
offset: anOffset
 	"Return the timezone with the given offset"
 	^ self timeZones 
		detect: [ :timeZone | timeZone offset = anOffset ]
		ifNone: [ self new offset: anOffset ]