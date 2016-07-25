event processing
onCandidateAdded
	"Select first candidate item in the list if there is no already selected
	candidate and candidate list has at least one candidate"
	(self hasSelected or: [ self candidates isEmpty ])
		ifFalse: [ self selectFirst ]