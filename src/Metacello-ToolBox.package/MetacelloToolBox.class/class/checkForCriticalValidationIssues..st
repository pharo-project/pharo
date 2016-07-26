utility
checkForCriticalValidationIssues: configurationBasename
	"Validate the named configuration and filter out all non-critical issues."
	<apiDocumentation>
	^ (MetacelloMCVersionValidator
		validateConfiguration: (Smalltalk at: (self configurationNameFrom: configurationBasename) asSymbol))
		select: [ :issue | issue isCritical ]