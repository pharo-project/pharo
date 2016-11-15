order
isActionHandled

	^ method methodClass isMeta
		and: [ method numArgs isZero and: [ method pragmas anySatisfy: [ :each | each keyword = #sampleInstance ] ] ]