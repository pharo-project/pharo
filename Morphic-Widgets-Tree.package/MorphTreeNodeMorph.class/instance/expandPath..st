testing
expandPath: anAssociation

	anAssociation ifNil: [ ^ false ].
	^ anAssociation head = self complexContents withoutListWrapper
		ifFalse: [ false ]
		ifTrue: [
			anAssociation tail ifNil: [ ^ true ].
			(self isExpanded not and: [ self canExpand ]) ifTrue: [
				self toggleExpandedState.
				container innerWidgetChanged ].
			self children anySatisfy: [:child | child expandPath: anAssociation tail ]]