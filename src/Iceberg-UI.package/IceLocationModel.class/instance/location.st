accessing
location
	^ locationInput text trim 
		ifNotEmpty: [ :value | value asFileReference ]
		ifEmpty: [ nil ]