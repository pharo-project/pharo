selecting
selected: aDoubleLink dueTo: anEvenType
	self assert: aDoubleLink isNotNil.
	self assert: aDoubleLink processor isNotNil.
	
	(self isSelected: aDoubleLink)
		ifTrue: [ ^ self ].
	
	self spotter
		after: [
			selected := aDoubleLink.
			self notifySelectedCandidateChanged: selected for: anEvenType]
		do: [ 		
			self spotter notifyCategorySelected: self selected processor ]
		ifTrue: [ (self isCategorySelected: aDoubleLink processor) not ]
	
	