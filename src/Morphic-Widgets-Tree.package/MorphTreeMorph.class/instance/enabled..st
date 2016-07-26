accessing
enabled: aBoolean
	enabled := aBoolean.
	self
		submorphsDo: [ :sm | 
			sm
				allMorphsDo: [ :m | 
					(m respondsTo: #enabled:)
						ifTrue: [ m enabled: aBoolean ] ] ].
	self changed: #enabled.
	self changed