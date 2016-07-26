private
calculateInfoList
	| answer list cvs cv |
	answer := OrderedCollection new.
	cvs := ''.
	(cv := self versionForInfoList) 
		ifNil:  [cvs := cv versionString, ' '].
	(list := self projects) 
		ifNotEmpty: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'projects --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self packages)
		ifNotEmpty: [ 
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'packages --').
			answer addAll: list.
			answer add: (MBLabelInfo new name: '') ].
	(list := self groups)
	 		ifNotEmpty: [
			answer add: (MBLabelInfo new name: ' -- ', cvs, 'groups --').
			answer addAll: list ].
	^ answer