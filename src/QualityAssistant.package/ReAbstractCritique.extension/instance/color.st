*qualityAssistant
color

	^ ({
	#error -> Color red .
	#warning -> Color yellow .
	#information -> Color blue
	 } asDictionary at: rule severity ifAbsent: [ super color ]) alpha: 0.1
