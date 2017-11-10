Extension { #name : #ThreadSafeTranscript }

{ #category : #'*SmartSuggestions' }
ThreadSafeTranscript >> sugsContext [
	^ SugsTranscriptContext model: self
]
