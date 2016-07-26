private
systemVersionInfo
	^ String streamContents: [ :stream |
			stream 
				print: SystemVersion current; 
				nextPutAll: ' - '; 
				nextPutAll: ZnConstants defaultServerString ]
	