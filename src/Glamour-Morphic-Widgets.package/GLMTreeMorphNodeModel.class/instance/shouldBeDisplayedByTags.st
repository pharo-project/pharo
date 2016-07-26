testing
shouldBeDisplayedByTags
	| nodeTags |
	self flag: 'it looks like this method does not really get called for children'.
"	self item mooseName = #'org::easymock' ifTrue: [self haltOnce ].
"	self containerTree tagsToFilterBy isEmpty ifTrue: [ ^ true ].
	self withContentsDo: [ :node | 
		nodeTags := self containerTree glamourPresentation tagsFor: node item to: #filter.
		(self containerTree tagsToFilterBy allSatisfy: [ :each | nodeTags includes: each ])
			ifTrue: [ ^ true ] ].
	^ false