finalization
finalizationList
	
	^ FinalizationList ifNil: [ FinalizationList := WeakFinalizationList new ].