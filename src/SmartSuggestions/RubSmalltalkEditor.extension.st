Extension { #name : #RubSmalltalkEditor }

{ #category : #'*SmartSuggestions' }
RubSmalltalkEditor >> smartSuggestions [
	"Open suggested actions based on the selection"

	SugsMenuBuilder showMenuFor: self
]

{ #category : #'*SmartSuggestions' }
RubSmalltalkEditor class >> suggestionsMenuOn: aBuilder [
	<RubSmalltalkCodeMenu>
	aBuilder
		item: 'Suggestions...';
		selector: #value;
		target: [ SugsMenuBuilder showMenuFor: aBuilder model ];
		keyText: 't ';
		order: 0;
		iconName: #smallAuthoringTools;
		with: [ SugsMenuBuilder buildContextMenuOn: aBuilder ]
]
