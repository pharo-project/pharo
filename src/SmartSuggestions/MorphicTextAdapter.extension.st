Extension { #name : #MorphicTextAdapter }

{ #category : #'*SmartSuggestions' }
MorphicTextAdapter >> sugsContext [
	^SugsMorphicTextAdapterContext model: self
]
