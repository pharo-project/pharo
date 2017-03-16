*EpiceaBrowsers
browseVersionsOf: aMethod
	"Browse all versions of aMethod from my head."
	
	^ (EpLogBrowserModel newWithLog: self)
			addFilter: (EpPluggableFilter isMethod: aMethod);
			refresh;
			openWithSpec