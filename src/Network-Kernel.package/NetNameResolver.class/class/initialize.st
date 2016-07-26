initialization
initialize
	"NetNameResolver initialize"
	"Note: On the Mac, the name resolver is asynchronous (i.e., Squeak can do other things while it is working), but can only handle one request at a time. On other platforms, such as Unix, the resolver is synchronous; a call to, say, the name lookup primitive will block all Squeak processes until it returns."

	"Resolver Status Values"
	ResolverUninitialized := 0.	"network is not initialized"
	ResolverReady := 1.			"resolver idle, last request succeeded"
	ResolverBusy := 2.			"lookup in progress"
	ResolverError := 3.			"resolver idle, last request failed"

	DefaultHostName := ''.
	HaveNetwork := nil.
	ResolverMutex := Mutex new
	
