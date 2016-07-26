testing
canContinue
	 " divein / really ugly, spotter-step does not really know why it was created or where it comes from :( "
	" another hack: diveIncategory must never continue because it just streams all items from the previous step. continuing in diveInCategory would duplicate all items! " 
	^ self step event == #diveIn
		or: [ self step event isNil ]