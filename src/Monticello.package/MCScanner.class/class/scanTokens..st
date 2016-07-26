public api
scanTokens: aString
	"compatibility"
	^ Array with: (self scan: aString readStream)