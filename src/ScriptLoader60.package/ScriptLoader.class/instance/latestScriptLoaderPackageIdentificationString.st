mc related utils
latestScriptLoaderPackageIdentificationString
	"ScriptLoader new latestScriptLoaderPackageIdentificationString"
	
	^  self allCurrentVersions detect: [:each | each matchesRegex: self theScriptLoaderPackageName, '.*' ]