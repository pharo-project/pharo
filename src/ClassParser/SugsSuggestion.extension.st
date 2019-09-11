Extension { #name : #SugsSuggestion }

{ #category : #'*ClassParser' }
SugsSuggestion class >> for: aClass [
	^ ((self subclassesForNode: aClass), (self subclassesForNode: CDNode)) collect: [:each |each new]
]
