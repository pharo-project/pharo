adding
add: link after: otherLink
	"Add otherLink  after link in the list. Answer aLink."

	| savedLink |

	savedLink := otherLink nextLink.
	otherLink nextLink: link.
	link nextLink:  savedLink.
	savedLink ifNotNil: [savedLink previousLink: link].
	link previousLink: otherLink.
	^link.
	
	