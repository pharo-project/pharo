adding
add: link before: otherLink
	| savedLink |
	firstLink == otherLink ifTrue: [^ self addFirst: link].
	otherLink 
		ifNotNil:[
			savedLink := otherLink previousLink.
			link nextLink: otherLink.
			link previousLink: savedLink.
			otherLink previousLink: link.
			savedLink ifNotNil: [savedLink nextLink: link]].
	^ self errorNotFound: otherLink