private
nodeAt: aPath ifPresent: presentBlock ifAbsent: absentBlock
	| current |
	current := self root.
	aPath do: [ :segment | 
		current isDirectory
			ifTrue: [ current := current fileEntryAt: segment ifAbsent: [ ^ absentBlock value ]]
			ifFalse: [ ^ absentBlock value ]].
	^ presentBlock value: current