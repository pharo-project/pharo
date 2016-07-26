scripting-private
resolvePort: aPortReference defaultPortName: aSymbol 
	^ aPortReference paneName = #outer 
		ifTrue: [
			(GLMPresentationBoundPort new) 
						presentation: self; 
						name: aPortReference portName;
						yourself] 
		ifFalse: [
			(self paneNamed: aPortReference paneName) port: aPortReference portName].