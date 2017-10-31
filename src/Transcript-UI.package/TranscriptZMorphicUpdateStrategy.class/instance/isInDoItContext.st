testing
isInDoItContext

	^(thisContext findContextSuchThat: [ :ctx | ctx methodSelector = #DoIt ]) isNotNil