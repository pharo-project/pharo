accessing
sourceCode: aString

	^ ((self browser paneNamed: #code) port: #text) value: aString