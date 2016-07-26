accessing
decodePlusAsSpace
	"Return if $+ on input should be decoded as Character space.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	^ decodePlusAsSpace ifNil: [ decodePlusAsSpace := true ]