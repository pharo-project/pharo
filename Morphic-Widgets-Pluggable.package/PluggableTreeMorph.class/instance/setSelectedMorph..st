selection
setSelectedMorph: aMorph
	selectedWrapper := aMorph complexContents.
	self selection: selectedWrapper.
	setSelectionSelector ifNotNil:[
		model 
			perform: setSelectionSelector 
			with: (selectedWrapper ifNotNil:[selectedWrapper item]).
	].