accessing
slice
	^ MCSliceInfo new
		issueNumber: self number asString;
		title: self title;
		yourself