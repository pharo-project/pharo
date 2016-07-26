accessing
requiredPackagesParallelCollect: aBlock withMessage: aMessage
	"load the remote dependencies of all packages in parallel"

	| packages progress results maxConcurrentConnections connectionThrottle addSemaphore |
	
	results := OrderedCollection new.
	addSemaphore := Semaphore forMutualExclusion.
	connectionThrottle := Semaphore new.
	progress := 0.
	maxConcurrentConnections := 5.
	maxConcurrentConnections timesRepeat: [ connectionThrottle signal ]. "make room for maxConcurrentConnections"
	packages := self requiredPackages.
	
	aMessage displayProgressFrom: 0 to: packages size during: [ :bar|
		packages do: [ :aPackage| 
			connectionThrottle wait. "no more than maxConcurrentConnections"
			[|result|
				[
					result := aBlock value: aPackage.
					addSemaphore critical: [ "update the non-threadsafe collection"
						results add: result.
						bar current: (progress := progress + 1)]
				] ensure: [ connectionThrottle signal. "free a connection" ].
			] fork ]].
	
	maxConcurrentConnections timesRepeat: [ connectionThrottle wait ]. "wait for the last connection to finish"
	
	^ results