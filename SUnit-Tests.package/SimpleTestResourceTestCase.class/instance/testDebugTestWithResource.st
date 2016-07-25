running
testDebugTestWithResource
	"The debug will raise an error if the resource is not set up properly."
	
	self clearOuterResourceStateDuring: [(self class selector: #setRun) debug]