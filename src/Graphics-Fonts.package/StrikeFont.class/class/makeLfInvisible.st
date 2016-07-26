character shapes
makeLfInvisible
	"
	Make line feed characters invisible
	StrikeFont makeLfInvisible
	"
	self allInstances do: [ :font | font makeLfInvisible ]