initialize-release
decodePlusAsSpace: boolean
	"When boolean is true, $+ on input will be decoded as Character space.
	Else $+ is treated as a normal character, filtered by the safe set.
	This is normally only done application/x-www-form-urlencoded data,
	but is is on by default anyway."
	
	decodePlusAsSpace := boolean