*renraku
banLevelCandidates
"returns code entities which can ban a rule for this entity"

	self methodNode ifNotNil: [ :mNode |
		mNode compiledMethod ifNotNil: [ :method |
			^ method banLevelCandidates ] ].
	
	^ #()