options
defaultOptions
	^ #(
	+ optionInlineMetaObject            "meta object is inlined by default."
	+ optionInlineCondition              "condition is inlined by default."
	- optionCompileOnLinkInstallation   "generate compiledMethod on link installation"
	- optionOneShot                     "remove link after first activation"
	- optionMetalevel                   "force level: 0 for the link"
	- optionDisabledLink                "links are active by default"
	)