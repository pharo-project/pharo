pharo - updates
update60413
	"self new update60413"
	self withUpdateLog: '19733 update UFFI to allow 64bits structures
	https://pharo.fogbugz.com/f/cases/19733'.
	self loadTogether: self script60413 merge: false.
	self loadConfiguration: 'UnifiedFFI' version: '0.26'.
FFICallout initializeTypeAliases.
FFIExternalStructure allSubclasses 
	select: [ :each | each package name = 'UnifiedFFI-Tests' ]
	thenDo: [ :each | each rebuildFieldAccessors ].
	self flushCaches.
