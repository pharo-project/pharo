accessing - selected
selectedCategory
	"Return a category processor that produced a currently selected candidate.
	Note: Since selected link can not be accessed if there is no selected one,
	always check #hasSelected before using me"
	<return: #GTSpotterProcessor>
	
	^ self selected processor