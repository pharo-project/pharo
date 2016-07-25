external resource management
registerAsExternalResource
	"Note, subclasses should implement #resourceData
	and #finalizeResourceData: on class side"
	self deprecated: 'Use #autoRelease instead.' on: '2016-01-22' in: #Pharo5.  
	FFIExternalResourceManager addResource: self