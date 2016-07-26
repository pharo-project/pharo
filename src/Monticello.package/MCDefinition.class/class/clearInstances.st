cleanup
clearInstances
	WeakArray removeWeakDependent: Instances.
	Instances := nil