private
addComment

	parseNode ~~ nil
		ifTrue: 
			[parseNode comment: currentComment.
			currentComment := nil]