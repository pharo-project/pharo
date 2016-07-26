testing
matchPath: anAssociation

	anAssociation ifNil: [ ^ nil ].
	^ anAssociation head = self complexContents withoutListWrapper
		ifFalse: [ nil ]
		ifTrue: [ | matchingChildren |
			anAssociation tail ifNil: [ ^ { self } ].
			matchingChildren := self children collect: [:child | child matchPath: anAssociation tail ].
			^ matchingChildren select: [ :e | e notNil ] ]