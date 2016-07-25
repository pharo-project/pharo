private
activate
	activeTranscript 
		ifNil: [
			Transcript class == ThreadSafeTranscript ifFalse: [ 
				ThreadSafeTranscript install 
			]
		]		
		ifNotNil: [ Transcript := activeTranscript ].
		
	SystemProgressMorph enable.