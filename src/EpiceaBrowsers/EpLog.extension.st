Extension { #name : #EpLog }

{ #category : #'*EpiceaBrowsers' }
EpLog >> browseEvents [
	"Browse all events from my head."
	
	^ EpLogBrowserPresenter openWithLog: self
]

{ #category : #'*EpiceaBrowsers' }
EpLog >> browseVersionsOf: aMethod [
	"Browse all versions of aMethod from my head."
	
	^ (EpLogBrowserPresenter newWithLog: self)
			addFilter: (EpPluggableFilter isMethod: aMethod);
			refresh;
			openWithSpec
]
