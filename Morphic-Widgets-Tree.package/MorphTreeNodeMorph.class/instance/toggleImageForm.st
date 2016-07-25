accessing
toggleImageForm
	^ isExpanded 
			ifTrue: [container expandedFormForMorph: self]
			ifFalse: [container notExpandedFormForMorph: self] 
				
		
