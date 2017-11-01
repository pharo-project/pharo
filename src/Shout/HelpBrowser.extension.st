Extension { #name : #HelpBrowser }

{ #category : #'*Shout-Styling' }
HelpBrowser >> shoutAboutToStyle: aPluggableShoutMorphOrView [ 
	^ (self rootTopic respondsTo: #canHaveSyntaxHighlighting)
				and: [self rootTopic canHaveSyntaxHighlighting]
]
