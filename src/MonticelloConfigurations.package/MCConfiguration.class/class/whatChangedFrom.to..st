utilities
whatChangedFrom: oldConfig to: newConfig
	"MCConfiguration
		whatChangedFrom: ReleaseBuilderPloppDeveloper config20060201PloppBeta
		to:  ReleaseBuilderPloppDeveloper config20060215premaster" 

	self whatChangedFrom: oldConfig to: newConfig on: Transcript.
	Transcript flush.