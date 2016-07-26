mocks
treeFrom: anArray
	| name id |
	name := anArray first.
	id := '00000000-0000-0000-0000-0000000000', (name asString size = 1 ifTrue: [name asString, '0'] ifFalse: [name asString]).
	^ MCVersionInfo
		name: name
		id: (UUID fromString: id)
		message: ''
		date: nil
		time: nil
		author: ''
		ancestors: (anArray size > 1 ifTrue: [(anArray second collect: [:ea | self treeFrom: ea])] ifFalse: [#()])