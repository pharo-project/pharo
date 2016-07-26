character shapes
makeLfVisible
	"
	Make line feed characters visible
	StrikeFont makeLfVisible
	"
	self allInstances do: [ :font | font makeLfVisible ]