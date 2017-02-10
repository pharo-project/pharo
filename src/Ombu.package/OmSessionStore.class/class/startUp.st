system startup
startUp
	"The #store accessor refreshes each instance since it will discover that the Session changed"

	self allInstancesDo: #store 