reshaping
rebase: originalScope to: newScope
	| newParent result fieldIndex |
	newParent := parentScope == originalScope
		ifTrue: [ newScope ]
		ifFalse: [ parentScope rebase: originalScope to: newScope ].
		
	result := self copy.
	result parentScope: newParent.
	
	fieldIndex := newParent firstFieldIndex.
	result do: [ :slot | 
		slot isVirtual ifFalse: [slot index: fieldIndex].
		fieldIndex := fieldIndex + slot size ].
	
	^ result
	