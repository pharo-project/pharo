system startup
shutDown
	"Send close to all our instances.
	Their streams will be reopened on first use later on."
	
	self allInstances do: #close