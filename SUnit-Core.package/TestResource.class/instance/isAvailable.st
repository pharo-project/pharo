testing
isAvailable
	"Override to provide information on the readiness of the resource.  Put state-changing behaviour in setUp and keep this a state-preserving check as far as possible.  Where setUp is guaranteed to provide a valid resource if it completes, there is no need to override this."
	
	^true