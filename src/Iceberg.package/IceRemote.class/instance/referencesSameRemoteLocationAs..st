testing
referencesSameRemoteLocationAs: another
	"Base/trivial implementation is that two urls are the same if they are equal, 
	but some subclasses have more complicated logic to detect two urls pointing to the same remote"
	^ self url = another url
	