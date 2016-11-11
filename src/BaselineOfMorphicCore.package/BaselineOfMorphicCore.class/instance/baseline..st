baseline
baseline: spec 
    
	<baseline>

	| repository | 
	
	repository := 'filetree://./pharo-core/src'. 

	spec for: #common do: [
		spec postLoadDoIt: #'postload:package:'.

		spec baseline: 'SUnit' with: [spec repository: repository].
		spec baseline: 'Display' with: [spec repository: repository].
		spec baseline: 'UnifiedFFI' with: [spec repository: repository].

		spec package: 'System-VMEvents'.
		spec package: 'System-Object Events'.
		spec package: 'Graphics-Canvas'.
		spec package: 'Morphic-Core'.
	].