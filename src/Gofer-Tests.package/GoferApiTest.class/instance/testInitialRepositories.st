tests
testInitialRepositories
	gofer := Gofer new.
	self assert: (gofer repositories size = 1).
	self assert: (gofer repositories first isKindOf: MCCacheRepository uniqueInstance class)