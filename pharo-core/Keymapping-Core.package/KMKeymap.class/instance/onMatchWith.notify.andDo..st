matching
onMatchWith: anEventBuffer notify: aMatchListener andDo: anAction
	(self matches: anEventBuffer)
			ifTrue: [ 
				(self matchesCompletely: anEventBuffer)
					ifTrue: [ self notifyCompleteMatchTo: aMatchListener buffer: anEventBuffer ]
					ifFalse: [ self notifyPartialMatchTo: aMatchListener ].
					anAction value.
				] 