shortcuts
methodShortcuts: event from: aMorph

	self selectedSelector
		ifNotNil:[
			event keyString = '<Cmd-b>' 		ifTrue: [ ^ self browseMethodFull ].
			event keyString = '<Cmd-d>' 		ifTrue: [ ^ self forgetMessage ].
			event keyString = '<Cmd-m>' 	ifTrue: [ ^ self browseImplementorsOfMessages ].
			event keyString = '<Cmd-n>' 		ifTrue: [ ^ self browseSendersOfMessages ].
			event keyString = '<Cmd-v>' 		ifTrue: [ ^ self browseVersions ].
			event keyString = '<Cmd-x>' 		ifTrue: [ ^ self removeMessage ]].