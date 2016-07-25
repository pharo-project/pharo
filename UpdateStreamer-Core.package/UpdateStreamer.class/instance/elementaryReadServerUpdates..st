private - actions
elementaryReadServerUpdates: urls
	"Scan the update server(s) for unassimilated updates. If the instance variable upToNumber is not nil, it represents the highest-numbered update to load.  This makes it possible to update only up to a particular point."

	"UpdateStreamer new elementaryReadServerUpdates"

	| str failed loaded this nextDoc updateName |
	(Smalltalk globals includesKey: #ScriptLoader)
		ifFalse: [ ^ self feedback: 'ScriptLoader is not present (this may happen if you have run cleanUpForProduction). ' ].
	Cursor wait
		showWhile: [ 
			| docQueue docQueueSema |
			loaded := 0.
			failed := nil.	"send downloaded documents throuh this queue"
			docQueue := SharedQueue new.	"this semaphore keeps too many documents from beeing queueed up at a time"
			docQueueSema := Semaphore new.
			5 timesRepeat: [ docQueueSema signal ].	"fork a process to download the updates"
			self retrieveUrls: urls ontoQueue: docQueue withWaitSema: docQueueSema.	"process downloaded updates in the foreground"
			[ 
			this := docQueue next.
			nextDoc := docQueue next.
			nextDoc = #failed
				ifTrue: [ failed := this ].
			failed isNil and: [ nextDoc ~= #finished ] ]
				whileTrue: [ 
					failed
						ifNil: [ 
							nextDoc isEmpty
								ifTrue: [ failed := this ] ].
					failed
						ifNil: [ 
							nextDoc peek asciiValue = 4
								ifTrue: [ failed := this ]	"pure object file" ].	"Must be fileIn, not pure object file"
					failed
						ifNil: [ 
							self shouldApplyChanges
								ifTrue: [ 
									updateName := (this findTokens: '/') last.
									ChangeSet newChangesFromStream: nextDoc named: updateName.
									SystemVersion current registerUpdate: updateName initialIntegerOrNil ]].
					loaded := loaded + 1.
					docQueueSema signal ] ].
	failed notNil & (urls size - loaded > 0)
		ifTrue: [ 
			str := loaded printString , ' new update file(s) processed.'.
			str := str , '\Could not load ' withCRs , (urls size - loaded) printString , ' update file(s).'
				, '\Starting with "' withCRs , failed , '".'.
			self feedback: str.
			UpdateFileNotLoaded signalFile: failed ].
	^ Array with: failed with: loaded