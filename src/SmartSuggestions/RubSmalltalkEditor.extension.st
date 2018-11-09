Extension { #name : #RubSmalltalkEditor }

{ #category : #'*SmartSuggestions' }
RubSmalltalkEditor >> smartSuggestions [
	"Open suggested actions based on the selection"

	SugsMenuBuilder showMenuFor: self
]
