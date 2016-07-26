housekeeping
fixObsoleteReferences
	"SmalltalkImage current fixObsoleteReferences.
	SystemNavigation new obsoleteBehaviors size > 0
		ifTrue: [ SystemNavigation new obsoleteBehaviors inspect.
			self error:'Still have obsolete behaviors. See inspector']"
		
	self
		garbageCollect;
		fixObsoleteBindings;
		fixObsoleteSharedPools.
	Behavior flushObsoleteSubclasses