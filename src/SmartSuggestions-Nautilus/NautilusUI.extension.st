Extension { #name : #NautilusUI }

{ #category : #'*SmartSuggestions-Nautilus' }
NautilusUI >> sugsContext [
	^ SugsNautilusContext model: self.
]
