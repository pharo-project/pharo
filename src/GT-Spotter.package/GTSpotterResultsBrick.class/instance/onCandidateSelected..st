event handling
onCandidateSelected: ann
	|processor categoryBrick itemBrick titleBrick scrollTo|

	processor := ann itemBrick processor.	
	categoryBrick := self categoryBrickOf: processor ifAbsent: [ ^ self ].
	itemBrick := ann itemBrick.
	titleBrick := categoryBrick titleBrick.
	scrollTo := (itemBrick candidateLink isFirst and: [ titleBrick isFirstInOwner ])
		ifTrue: [ titleBrick ]
		ifFalse: [ itemBrick ].
	self scrollTo: scrollTo