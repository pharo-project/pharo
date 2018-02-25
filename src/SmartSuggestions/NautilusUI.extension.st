Extension { #name : #NautilusUI }

{ #category : #'*SmartSuggestions' }
NautilusUI >> sugsContext [
	^ SugsNautilusContext model: self.
]
