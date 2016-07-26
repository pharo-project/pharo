registration
register: anObject as: aNameSelector
	"Register a tool class under given name"
	tools at: aNameSelector put: anObject.
	WorldState defaultWorldMenu