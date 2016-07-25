operations
delete: anInterval
	"Delete portion of text enclosed by interval.
	Answer the insertion position pointing to the text where
	interval was residing"
	
	^ anInterval minMaxDo: [ :min :max | min deleteUpTo: max ]