pharo - updates
update60303
	"self new update60303"
	
(MCWorkingCopy forPackage: (MCPackage named: FLPlatform package name)) unload.
	self withUpdateLog: '18330 Fuel still has platform classes in Pharo6 like FLPharo11Platform.
	https://pharo.fogbugz.com/f/cases/18330

19381 KMSingleKeyCombination should be not printed with forced uppercases
	https://pharo.fogbugz.com/f/cases/19381

19382 #add:finalizing: in AlienWeakTable sends unexistent message #copyGrownBy
	https://pharo.fogbugz.com/f/cases/19382'.
	self loadTogether: self script60303 merge: false.
	self loadConfiguration: 'Fuel' version: '2.1.8'.
self loadConfiguration: 'UnifiedFFI' version: '0.24.3'.
Smalltalk fixObsoleteBindings.
	self flushCaches.
