emitting code
offsetWriteFieldAt: offsetVariableName with: valueName
	| cr tab |
	cr := String cr.
	tab := String tab.
	^ String streamContents: [ :stream | 
		stream 
			<< 'handle ' << cr 
			<< tab << tab << 'pointerAt: ' << offsetVariableName << cr 
			<< tab << tab << 'put: (anObject ' << cr 
			<< tab << tab << tab << 'ifNotNil: [ ExternalAddress fromAddress: anObject thunk address ]' << cr
			<< tab << tab << tab << 'ifNil: [ ExternalAddress null ])' ]