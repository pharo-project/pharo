private-reading
unzipTimestamp: dosTimestampInteger

	^ [ DateAndTime fromDosTimestamp: dosTimestampInteger ]
		on: Error
		do: [ "Assume we're dealing with the old, incorrect Squeak format"
			DateAndTime fromSeconds: 2492992800 + dosTimestampInteger ]