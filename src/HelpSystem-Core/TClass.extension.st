Extension { #name : #TClass }

{ #category : #'*HelpSystem-Core' }
TClass >> asHelpTopic [
	^SystemReference forClass: self
]
