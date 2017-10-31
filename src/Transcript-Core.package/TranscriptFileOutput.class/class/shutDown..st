system startup
shutDown: quitting
	"
		Send close to all instances.
		Their streams will be reopened on first use later on.
	"
	
	self allSubInstancesDo: [ :fileOutput | fileOutput closeStream ]