Extension { #name : #MCTool }

{ #category : #'*SmartSuggestions' }
MCTool >> sugsContext [
	
	^ SugsMonticelloToolContext model: self
]
