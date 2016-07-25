ston-core
fromSton: stonReader
	stonReader parseMapDo: [ :key :value |
		key = #username ifTrue: [ username := value ].
		key = #password ifTrue: [ password := value ].
		key = #enabled ifTrue: [ enabled := value ] ]
	