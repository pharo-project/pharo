Extension { #name : #DiffMorph }

{ #category : #'*SmartSuggestions' }
DiffMorph >> sugsContext [
	^ SugsDiffModelContext model: self 
]
