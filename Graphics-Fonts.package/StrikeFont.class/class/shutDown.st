system startup
shutDown
	"StrikeFont shutDown"
	"Deallocate synthetically derived copies of base fonts to save space"
	self allSubInstancesDo: [ :sf | sf reset ].
	StrikeFontSet allSubInstancesDo: [ :sf | sf reset ].
