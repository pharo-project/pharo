private
selectionChangedUpdateStatus: ann
	"Updates the status of selection (informs new selections and deselections)"
	self setSelectionListSelector ifNil: [ ^ self ].	
	
	ann oldSelectedRowIndexes do: [ :each | 
		self model 
			perform: self setSelectionListSelector 
			with: each
			with: false ].
	
	ann newSelectedRowIndexes do: [ :each | 
		self model 
			perform: self setSelectionListSelector 
			with: each
			with: true ].
