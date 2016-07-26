accessing
selected
	"Return a candidate represented by double link to allow fast access to
	next and previous candidate. In order to get actual candidate object behind
	the link send #candidate to it.

	selected can not be nil (always use hasSelected before using me)"
	<return: #GTSpotterCandidateLink>
	self assert: selected isNotNil.
	
	^ selected
	
	" ifNil: [
		selected := (self candidates isEmpty
			ifTrue: [ ^ nil ]
			ifFalse: [ self candidates head ]).
		self assert: selected isNotNil.
		self notifySelectedCandidateChanged: selected.
		self spotter notifyCategorySelected: self selected processor.
		selected ]"