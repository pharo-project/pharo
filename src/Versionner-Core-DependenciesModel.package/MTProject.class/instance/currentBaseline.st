accessing
currentBaseline
	| baselines |
	
	baselines := self configurationClass project versions select: [:aVersion | aVersion blessing == #baseline].
	^baselines last