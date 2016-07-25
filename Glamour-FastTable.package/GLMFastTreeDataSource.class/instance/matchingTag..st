tag
matchingTag: anItem
	| elementTags |
	elementTags := self glamourPresentation tagsFor: anItem data to: #filter.
	((anItem children collect: [ :it | self matchingTag: it ]) reject: #isNil)
		ifNotEmpty: [ :coll | 
			^ (self newStaticItemFrom: anItem)
				children: coll asArray;
				yourself ].
	^ (self tagsToFilterBy allSatisfy: [ :each | elementTags includes: each ])
		ifFalse: [ nil ]
		ifTrue: [ self newStaticItemFrom: anItem ]