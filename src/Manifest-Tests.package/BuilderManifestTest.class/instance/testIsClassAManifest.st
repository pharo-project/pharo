tests
testIsClassAManifest
	"self debug: #testIsClassAManifest"
	
	self deny: Point isManifest.
	TheManifestBuilder of: MFClassA. 
	self assert: ( (Smalltalk at: #ManifestManifestResourcesTests) isManifest)