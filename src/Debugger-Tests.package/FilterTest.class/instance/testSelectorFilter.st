tests
testSelectorFilter
	| doItFilter nilMessagesFilter |
	
	doItFilter := SelectorFilter forSelector: #doIt.
	self assert: (doItFilter shouldDisplay: context).
	self assert: (doItFilter shouldDisplay: nilContext).
	nilMessagesFilter := SelectorFilter forSelectors: (OrderedCollection with: #isNil with: #ifNil:).
	self assert: (nilMessagesFilter shouldDisplay: context)	"self deny: (nilMessagesFilter shouldDisplay: otherSession context)"