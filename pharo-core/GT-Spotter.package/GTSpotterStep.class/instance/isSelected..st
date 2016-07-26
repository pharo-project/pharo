testing
isSelected: aCandidateLink
	"Return true if aCandidateLink is currently selected,
	false otherwise"
	<return: #Boolean>
	
	^ self hasSelected and: [ self selected == aCandidateLink ]