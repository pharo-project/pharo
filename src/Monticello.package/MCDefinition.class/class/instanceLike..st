instance creation
instanceLike: aDefinition
	Instances ifNil: [ Instances := WeakSet new ].
	InstancesWriteLock ifNil: [ InstancesWriteLock := Semaphore forMutualExclusion ].
	^ (Instances like: aDefinition) 
		ifNil: [
			InstancesWriteLock critical: [ Instances add: aDefinition ]]