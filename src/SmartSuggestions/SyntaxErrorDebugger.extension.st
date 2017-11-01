Extension { #name : #SyntaxErrorDebugger }

{ #category : #'*SmartSuggestions' }
SyntaxErrorDebugger >> sugsContext [
	^ SugsSyntaxErrorDebuggerContext model: self
]
