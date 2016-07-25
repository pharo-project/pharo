accessing
specialKeys
	^specialKeys
		ifNil: [ specialKeys :=
				{1->'home'.
				3->'enter'.
				4->'end'.
				8->'backspace'.
				9-> 'tab'.
				11->'page up'.
				12->'page down'.
				27->'escape'.
				28->'left arrow'.
				29->'right arrow'.
				30->'up arrow'.
				31->'down arrow'.
				127->'delete'
				} asDictionary.]