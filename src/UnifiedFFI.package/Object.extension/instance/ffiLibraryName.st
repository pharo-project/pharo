*UnifiedFFI
ffiLibraryName
	self flag: 'HACK: avoid direct subclassResponsibility to not break RB test cases..'.
	^ SubclassResponsibility signalFor: thisContext selector