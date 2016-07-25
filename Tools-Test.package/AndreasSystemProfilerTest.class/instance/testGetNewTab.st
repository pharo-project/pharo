tests tally
testGetNewTab
	| tabs |
	
	self assert: (tally getNewTabsFor: #()) = #().
	self assert: (tally getNewTabsFor: #(' ')) =  #(' ').
	
	tabs := OrderedCollection new.
	tabs add: '['.
	(tally maxTabs + 1) timesRepeat: [ tabs add: ' ' ].
	self assert: (tally getNewTabsFor: tabs) asArray =  #('[' '[').