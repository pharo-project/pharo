private
resetEnvironment
	"I send an event when I reset the environment"
	self environment: self defaultEnvironment.
	self triggerEvent: #resetEnvironment.