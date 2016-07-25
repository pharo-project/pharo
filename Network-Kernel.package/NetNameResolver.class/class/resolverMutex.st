private
resolverMutex
	"This must have been initialized by class initialization. 
	If a failure occurs due to mutex not being properly initialized, do NOT solve it by lazy initialization, or you WILLl introduce a race condition"
	^ResolverMutex