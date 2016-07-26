*System-Finalization
mourn
	"Sent by the new Finalization mechanism.
	All weak objects (instances of weak classes) and Ephemerons may be mourned if they point to an object that needs to be collected.
	
	This default implementation does nothing. To override in specific subclasses."