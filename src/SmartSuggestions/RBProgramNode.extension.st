Extension { #name : #RBProgramNode }

{ #category : #'*SmartSuggestions' }
RBProgramNode >> suggestions [
	^SugsSuggestion for: self class
]

{ #category : #'*SmartSuggestions' }
RBProgramNode >> sugsMenuLabel [
	^ self formattedCode
]
