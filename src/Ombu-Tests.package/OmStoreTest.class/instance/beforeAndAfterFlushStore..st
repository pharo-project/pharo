running
beforeAndAfterFlushStore: aBlock 

	aBlock value.
	store flush. "Reason: It's worth testing again, after flush."
	aBlock value.