adding
add: link after: otherLinkOrObject
	"Add otherLink  after link in the list. Answer aLink."

	| otherLink |
	otherLink := self linkAt: (self indexOf: otherLinkOrObject).
	^ self add: link afterLink: otherLink