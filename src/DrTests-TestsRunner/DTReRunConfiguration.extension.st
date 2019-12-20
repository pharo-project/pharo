Extension { #name : #DTReRunConfiguration }

{ #category : #'*DrTests-TestsRunner' }
DTReRunConfiguration >> handleResults: aPluginResult for: aPlugin [
	^ aPlugin handleReRunResult: aPluginResult forConfiguration: self
]
