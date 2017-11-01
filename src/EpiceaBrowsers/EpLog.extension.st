Extension { #name : #EpLog }

{ #category : #'*EpiceaBrowsers' }
EpLog >> browseEvents [
	"Browse all events from my head."
	
	^ EpLogBrowserModel openWithLog: self
]

{ #category : #'*EpiceaBrowsers' }
EpLog >> browseVersionsOf: aMethod [
	"Browse all versions of aMethod from my head."
	
	^ (EpLogBrowserModel newWithLog: self)
			addFilter: (EpPluggableFilter isMethod: aMethod);
			refresh;
			openWithSpec
]
