menu
compareToOtherVersion
	| labels versions index selected |
	versions := self buildChangeList.
	labels := versions collect: [ :version | version stamp ].
	index := UIManager default chooseFrom: labels.
	index > 0
		ifFalse: [ ^ self ].
	selected := versions at: index.
	self compareTo: selected withLabel: selected stamp