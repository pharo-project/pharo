character shapes
makeTabVisible
	"
	Make tab characters visible
	StrikeFont makeTabVisible
	"
	self allInstances do: [ :font | font makeTabVisible ]