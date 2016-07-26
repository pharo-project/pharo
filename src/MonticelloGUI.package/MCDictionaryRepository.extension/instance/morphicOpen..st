*MonticelloGUI
morphicOpen: aWorkingCopy
	| names index infos |
	infos := self sortedVersionInfos.
	infos isEmpty ifTrue: [^ self inform: 'No versions'].
	names := infos collect: [:ea | ea name].
	index := UIManager default chooseFrom: names title: 'Open version:'.
	index = 0 ifFalse: [(self versionWithInfo: (infos at: index)) open]