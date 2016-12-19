pharo - updates
update60329
	"self new update60329"
	self withUpdateLog: '19476 LabelModel default color needs to be theme text color (not hardcoded black)
	https://pharo.fogbugz.com/f/cases/19476

19473 empty package Spec-SelectEntity can be unloaded
	https://pharo.fogbugz.com/f/cases/19473

19453 NeoUUIDGenerator>>testTwoSameGenerator - intermittent CI failure
	https://pharo.fogbugz.com/f/cases/19453

19471 classAffected method for MethodAnnouncement to be polymorphic with other system changes
	https://pharo.fogbugz.com/f/cases/19471'.
	self loadTogether: self script60329 merge: false.
	
ScriptLoader new unloadPackageNamed: 'Spec-SelectEntity'.
	self flushCaches.
