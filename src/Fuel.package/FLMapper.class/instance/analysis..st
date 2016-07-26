accessing
analysis: anAnalysis

	analysis := anAnalysis.
	next ifNotNil: [ next analysis: anAnalysis ]