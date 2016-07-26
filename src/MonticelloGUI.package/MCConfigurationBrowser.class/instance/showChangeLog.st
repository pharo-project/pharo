actions
showChangeLog
	self pickConfig ifNotNil: [:oldConfig |
		Transcript dependents isEmpty
			ifTrue: [Transcript open]
			ifFalse: [Transcript dependents do: [:ea | 
				ea isSystemWindow ifTrue: [ea activate]]].
		Cursor wait showWhile: [
			MCConfiguration whatChangedFrom: oldConfig to: configuration on: Transcript.
			Transcript flush]]