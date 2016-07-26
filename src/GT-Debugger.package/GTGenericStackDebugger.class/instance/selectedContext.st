accessing context
selectedContext

	^ ((self browser paneNamed: #stack) port: #selection) value