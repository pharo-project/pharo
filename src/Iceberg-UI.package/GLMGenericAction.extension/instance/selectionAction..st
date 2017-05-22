*Iceberg-UI
selectionAction: actionSelector
	"action selector will be sent to the presentation selection. 
	If it accepts one argument, the presentation will be sent."

	^ self
		action: [ :presentation | 
			presentation selection
				ifNotNil: [ :sel | (MessageSend receiver: sel selector: actionSelector) cull: presentation ] ];
		enabledCondition:
				[ :presentation | "This is a minimal condition, that can be overriden (refined) later" presentation selection notNil ]