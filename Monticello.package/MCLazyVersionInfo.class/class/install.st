class initialization
install
	"Install the lazy mc versions on..."

	"self install"

	MCWorkingAncestry
		allInstancesDo: [ :each | each ancestors: (each ancestors collect: [ :e | MCLazyVersionInfo from: e ]) ].
	MCFileBasedRepository flushAllCaches.
	Smalltalk garbageCollect