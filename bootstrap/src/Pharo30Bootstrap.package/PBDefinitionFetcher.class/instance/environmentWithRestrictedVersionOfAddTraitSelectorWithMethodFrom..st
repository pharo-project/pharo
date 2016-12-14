patching
environmentWithRestrictedVersionOfAddTraitSelectorWithMethodFrom: aRFiEnvironment
	"TBehavior>>#addTraitSelector:withMethod: deals with sources files that is not available in the bootstrap (low-level reason is that EncodedCharSet is not initialized)."
	| edit |
	edit := aRFiEnvironment ficusPackages
		editAtPath: FiPath root / #Traits / #contents / #TBehavior / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'addTraitSelector: aSymbol withMethod: aCompiledMethod
	| source method |
	self ensureLocalSelectors.
	method := aCompiledMethod copy.
	method selector: aSymbol.
	method methodClass: self.
	self isTrait ifFalse: [ method properties at: #traitSource put: aCompiledMethod ].
	self basicAddSelector: aSymbol withMethod: method') ].
	^ (aRFiEnvironment ficusPackages resultOf: edit) asRFiEnvironment.
