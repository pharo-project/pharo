tests
testPanePushAnnouncement
	| count currentAnn paneMorph |
	
	count := 0.
	paneMorph := self createSimplePaneMorph.
	pagerModel announcer when: GLMPagePushed do: [ :ann | count := count + 1. currentAnn := ann ].
	
	pagerModel pushPane: paneMorph smooth: false.
	self assert: count equals: 1.
	self assert: currentAnn morph equals: paneMorph.
	
	paneMorph := self createSimplePaneMorph.
	pagerModel pushPane: paneMorph smooth: false.
	self assert: count equals: 2.
	self assert: currentAnn morph equals: paneMorph.
	
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	pagerModel pushPane: self createSimplePaneMorph smooth: false.
	self assert: count equals: 4.