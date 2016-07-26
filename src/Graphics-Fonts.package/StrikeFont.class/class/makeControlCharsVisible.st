character shapes
makeControlCharsVisible
	"
	Make normally not visible characters, visible
	StrikeFont makeControlCharsVisible
	"
	self allInstances do: [ :font | font makeControlCharsVisible ]