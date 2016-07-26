accessing
selected: aBoolean
	"Set the state taking account of the intermediate states."

	(self state == #pressed or: [self state == #repressed])
		ifTrue: [self state: (aBoolean ifTrue: [#repressed] ifFalse: [#pressed])]
		ifFalse: [self state: (aBoolean ifTrue: [#on] ifFalse: [#off])]