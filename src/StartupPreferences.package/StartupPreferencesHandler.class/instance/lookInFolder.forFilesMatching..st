private
lookInFolder: folder forFilesMatching: pattern

	| result files |
	result := OrderedCollection new.
	folder isDirectory ifFalse: [ ^ nil ].
	
	files := folder filesMatching: pattern.
	files do: [ :e | result add: e ].

	^ result isEmpty
		ifTrue: [ nil ]
		ifFalse: [ result ]