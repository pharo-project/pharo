opening
spyOn: aBlock 
	^ (self new withBlockCodePane: false; withToolBar: false; yourself)
		openOnBlock: aBlock 