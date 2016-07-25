*UnifiedFFI-Deprecated50
gcpointer
	"Same as #pointer (see its comment for detals), but contents are garbage collected automatically"
	self deprecated: 'Use "pointer autoRelease" instead.' on: '2016-01-22' in: #Pharo5.  
	^ (ExternalAddress gcallocate: self size)
		pointerAt: 1 put: self;
		yourself