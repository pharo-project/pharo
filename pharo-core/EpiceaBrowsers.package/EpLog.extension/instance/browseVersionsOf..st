*EpiceaBrowsers
browseVersionsOf: aMethod
	"Browse all versions of aMethod from my head."
	
	^ (EpLogBrowser newWithLog: self)
			addFilter: (EpPluggableFilter isMethod: aMethod);
			refresh;
			open