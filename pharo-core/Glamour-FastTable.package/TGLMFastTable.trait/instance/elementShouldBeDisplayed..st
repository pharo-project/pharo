private
elementShouldBeDisplayed: anObject
	| elementTags |
	self tagsToFilterBy ifEmpty: [ ^ true ].
	elementTags := self glamourPresentation tagsFor: anObject to: #filter.
	^ self tagsToFilterBy allSatisfy: [ :each | elementTags includes: each ]