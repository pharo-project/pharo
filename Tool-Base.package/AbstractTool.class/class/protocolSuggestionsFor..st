private
protocolSuggestionsFor: aClass

	| allExistingProtocols interestingProtocols reject |
	reject := Set new.
	reject
		addAll: aClass organization allCategories;
		add: AllProtocol defaultName;
		add: Protocol nullCategory;
		add: Protocol unclassified.
	allExistingProtocols := SystemNavigation default allExistingProtocolsFor: aClass isMeta not.
	
	interestingProtocols := allExistingProtocols reject: [ :e | reject includes: e ].
	^ interestingProtocols asOrderedCollection sort: [ :a :b | a asLowercase < b asLowercase ].