shortcuts
changeSetShortcuts: event from: aMorph

	self selectedChangeSet
		ifNotNil:[
			event keyString = '<Cmd-b>' 		ifTrue: [ ^ self browseChangeSet ].
			event keyString = '<Cmd-B>' 		ifTrue: [ ^ self openChangeSetBrowser ].
			event keyString = '<Cmd-m>' 	ifTrue: [ ^ self setCurrentChangeSet ].
			event keyString = '<Cmd-n>' 		ifTrue: [ ^ self newSet ].
			event keyString = '<Cmd-o>' 		ifTrue: [ ^ self fileOut ].
			event keyString = '<Cmd-r>' 		ifTrue: [ ^ self rename ].
			event keyString = '<Cmd-p>' 		ifTrue: [ ^ self addPreamble ].
			event keyString = '<Cmd-x>' 		ifTrue: [ ^ self remove ]].
			
	event keyString = '<Cmd-f>' 				ifTrue: [ ^ self findChangeSet ].
		