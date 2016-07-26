accessing
pageSearchText: aString
	| founds chosen chosenNode |
	pageSearchText := aString.
	self changed: #pageSearchText.
	founds := OrderedCollection new.
	self nodeList doWithIndex: [:n :idx |
		(n includesSubstringAnywhere: pageSearchText)
			ifTrue: [founds add: idx -> n]].
	founds ifEmpty: [ ^ self flash ].
	founds size > 1 
		ifTrue: [ 
			chosen := 
				UIManager default 
					chooseFrom: (founds collect: [:l | l value] ) 
					values: (founds collect: [:l | l key] ) 
					lines: nil 
					title: ''.
			chosen ifNil: [^self]]
		ifFalse: [chosen := founds first key].
	self currentPage: (self pageOfNodeIndex: chosen).
	chosenNode := self nodeList at: chosen.
	treeMorph scroller 
		submorphsDo: [:sm | 
			sm complexContents ==  chosenNode
				ifTrue: [treeMorph listManager setSelectedMorph: sm.
					^ treeMorph scrollSelectionIntoView]]
