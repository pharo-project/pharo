brick-properties-events
announce: aSymbol event: anEvent

	self announcer subscriptions glmSubscriptions do: [ :each |
		each announcementClass = aSymbol ifTrue: [
			(each handlesAnnouncement: aSymbol ) ifTrue: [
				[ each action cull: anEvent cull: self ] 
					on: UnhandledError fork: [:ex | ex pass ] ] ] ]