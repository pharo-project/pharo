shortcuts
classShortcuts: event from: aMorph

	self selectedClass
		ifNotNil:[
			event keyString = '<Cmd-b>' 		ifTrue: [ ^ self browseMethodFull ].
			event keyString = '<Cmd-d>' 		ifTrue: [ ^ self forgetClass ].
			event keyString = '<Cmd-x>' 		ifTrue: [ ^ self removeClass ]].