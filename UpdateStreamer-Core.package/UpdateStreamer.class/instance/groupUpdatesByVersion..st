private - actions
groupUpdatesByVersion: updateContents
	"Parse the contents of updates.list into {{vers. {fileNames*}}*}, and return it."
	
	| sections vers strm line fileNames |
	self flag: #shouldUseADictionary.
	sections := OrderedCollection new.
	fileNames := OrderedCollection new: 1000.
	strm := updateContents readStream.
	[ strm atEnd ] whileFalse: 
		[ line := strm nextLine.
		line notEmpty ifTrue: 
			[ line first = $# 
				ifTrue: 
					[ vers ifNotNil: [ sections addLast: {  vers. (fileNames asArray)  } ].
					"Start a new section"
					vers := line allButFirst.
					fileNames resetTo: 1 ]
				ifFalse: [ line first = $* ifFalse: [ fileNames addLast: line ] ] ] ].
	vers ifNotNil: [ sections addLast: {  vers. (fileNames asArray)  } ].
	^ sections 
	