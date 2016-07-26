filein/out
hasReportableSlip: aMethod
	"Answer whether the receiver contains anything that should be brought 
	to the attention of the author when filing out. Customize the lists here 
	to suit your preferences. If slips do not get reported in spite of your 
	best efforts here, make certain that the Preference 'checkForSlips' is set 
	to true."

	aMethod containsHalt ifTrue: [ ^true ].
	#(#flag:) do: [ :aLit | (aMethod hasLiteral: aLit) ifTrue: [ ^ true ] ].
	((aMethod literals 
		select: #isVariableBinding
		thenCollect: #key) includes: #Transcript) ifTrue: [ ^true ].
	^ false