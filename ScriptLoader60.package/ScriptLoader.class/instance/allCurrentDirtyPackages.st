mc related utils
allCurrentDirtyPackages
	"ScriptLoader new allCurrentDirtyPackages"
	"return all the current dirty packages even the ones that we do not want to save"

	
	^  self allCurrentPackages select: [:each | each needsSaving].

	