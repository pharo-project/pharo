enumerating
entriesSelect: aBlock
	| entries |
	entries := OrderedCollection new.
	self allFilesStatusDo: [ :path :status :payload |
		(aBlock value: path value: status) ifTrue: [ entries add: {path. status. payload} ].
		0 ].
	
	^ entries