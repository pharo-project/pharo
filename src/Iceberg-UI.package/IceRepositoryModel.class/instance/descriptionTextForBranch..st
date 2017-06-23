accessing
descriptionTextForBranch: aBranch
	| text |
	
	self flag: #todo. "This method is (almost) duplicated in
	IceHistoryBrowser>>descriptionTextForBranch:repository:. This can be solved by 
	using an IceBranchModel instead the branch directly"	
	text := aBranch name asText.
	(self isCurrentBranch: aBranch)
		ifTrue: [ text allBold ].
		
	^ text