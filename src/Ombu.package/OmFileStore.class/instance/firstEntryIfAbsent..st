accessing
firstEntryIfAbsent: absentBlock

	^ [ super firstEntryIfAbsent: absentBlock ] 
			on: Error 
			do: absentBlock