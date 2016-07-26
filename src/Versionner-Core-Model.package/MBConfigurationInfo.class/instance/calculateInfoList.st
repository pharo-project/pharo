private
calculateInfoList
	| answer list cvs cv |
	answer := OrderedCollection new.
	[self project ifNil: [ ^answer ]] on: Error do: [:ex | ^answer ].
	answer add: (MBLabelInfo new name: ' -- baselines --').
	answer addAll: (self trimVersionInfos: self baselines).
	answer add: (MBLabelInfo new name: '').
	answer add: (MBLabelInfo new name: ' -- versions --').
	answer addAll: (self trimVersionInfos: self versions).
	answer add: (MBLabelInfo new name: '').
	cvs := ''.
	(cv := self currentVersion) ~~ nil
		ifTrue:  [cvs := cv versionString, ' '].
	(list := self projects) notEmpty
		ifTrue: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'projects --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self packages) notEmpty
		ifTrue: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'packages --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self groups) notEmpty
		ifTrue: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'groups --').
			answer addAll: list].
	^ answer