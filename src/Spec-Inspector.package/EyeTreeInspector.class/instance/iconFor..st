api
iconFor: anEyeElement
	anEyeElement withErrorsDo: [ :error |
		^ self iconFor: anEyeElement error: error ].
		
	^ [ anEyeElement icon ]
		on: Error do: [ :error |
			self iconFor: anEyeElement error: error ]