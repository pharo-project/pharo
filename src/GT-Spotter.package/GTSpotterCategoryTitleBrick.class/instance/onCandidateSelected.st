event-handling
onCandidateSelected
	
	(self stepModel isCategorySelected: self category)
		ifTrue: [ self showActionbar ]
		ifFalse: [
			"well, we need to check first if there are not hovered candidates for my category"
			(self isSelected not and: [
				self owner isNotNil and: [
					(self owner owner categories includesKey: self category) and: [ 
						(self owner owner categoryBrickOf: self category) isSelected not  ] ] ] )
			ifTrue: [ self hideActionbar ] ]