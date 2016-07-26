character shapes
makeTabInvisible
	"
	Make tab characters invisible
	StrikeFont makeTabInvisible
	"
	self allInstances do: [ :font | font makeTabInvisible ]