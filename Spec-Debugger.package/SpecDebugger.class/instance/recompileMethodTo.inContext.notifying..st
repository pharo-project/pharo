actions
recompileMethodTo: aString inContext: aContext notifying: aNotifyer

	^ aContext 
		ifNil: [ false ]
		ifNotNil: [
			self session recompileMethodTo: aString inContext: aContext notifying: aNotifyer ]