spying
spyOn: aBlock reportOtherProcesses: aBoolean cutoff: aNumber openResultWindow: openResultWindow

	^ self 
		spyOn: aBlock 
		reportOtherProcesses: aBoolean 
		cutoff: aNumber 
		openResultWindow: openResultWindow 
		closeAfter: true