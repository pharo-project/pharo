Extension { #name : #Workspace }

{ #category : #'*SmartSuggestions' }
Workspace >> sourceTextArea [
	^ self textView
]

{ #category : #'*SmartSuggestions' }
Workspace >> sugsContext [
	^ SugsWorkspaceContext model: self
]
