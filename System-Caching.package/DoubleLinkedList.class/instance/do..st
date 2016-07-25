enumerating
do: block
	"Execute block for each of my elements."

	self linksDo: [ :each | block value: each value ]