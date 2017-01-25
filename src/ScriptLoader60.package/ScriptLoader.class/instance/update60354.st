pharo - updates
update60354
	"self new update60354"
	self withUpdateLog: '19596 move #outerMostContext to the KernelPackage
	https://pharo.fogbugz.com/f/cases/19596

19597 move #definitionForNautilus &co. to the Kernel
	https://pharo.fogbugz.com/f/cases/19597

19595 Protocol *GT-SpotterExtensions-Core-private in MCWorkingCopy should be moved to accessing
	https://pharo.fogbugz.com/f/cases/19595

19594 isTestMethods &co. should be SUnit extensions, not part of Nautilus
	https://pharo.fogbugz.com/f/cases/19594'.
	self loadTogether: self script60354 merge: false.
	self flushCaches.
