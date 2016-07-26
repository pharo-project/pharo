tests
testAutoTitle
	| actualTitleFromTracker |
	actualTitleFromTracker := 'Spec-ify DropListModel / TextModel'.
	self assert: issue title equals: actualTitleFromTracker