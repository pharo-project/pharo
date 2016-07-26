adding
add: aLinkOrObject afterLink: otherLink

	"Add otherLink  after link in the list. Answer aLink."

	| savedLink aLink |
	lastLink == otherLink ifTrue: [^ self addLast: aLinkOrObject].
	savedLink := otherLink nextLink.
	aLink := aLinkOrObject asLink.
	otherLink nextLink: aLink.
	aLink nextLink:  savedLink.
	^aLink.