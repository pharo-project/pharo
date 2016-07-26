morphic ui
pickConfig
	self pickRepository ifNotNil: [:repo |
		| configs index |
		configs := Cursor wait showWhile: [
			repo allFileNames
				select: [:ea | MCMcmReader canReadFileNamed: ea]
				thenCollect: [:ea | ea copyUpToLast: $.]].
		configs isEmpty ifTrue: [^self inform: 'no configs found in ', repo description].
		configs := configs asSortedCollection: [:a :b |
			(a copyAfterLast: $.) asNumber > (b copyAfterLast: $.) asNumber].
		index := UIManager default chooseFrom: configs title: 'config:' translated.
		index = 0 ifFalse: [^Cursor wait showWhile: [
			repo versionFromFileNamed: (configs at: index), '.', MCMcmReader extension]]].
	^nil