registration
registerTo: aModel

	aModel announcer
   	 when: NautilusMethodSelected 
	 send: #noteExectuted:
	 to: self