Extension { #name : #PluggableTextMorph }

{ #category : #'*SmartSuggestions' }
PluggableTextMorph >> smartSuggestions [
	self handleEdit: [textMorph editor smartSuggestions]
]
