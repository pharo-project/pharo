actions
collectCoverageFor: methods
	| wrappers suite |
	wrappers := methods collect: [ :each | TestCoverage on: each ].
	suite := self
		resetResult;
		suiteForAllSelected.
	
	[ wrappers do: [ :each | each install ].
	[ self runSuite: suite ] ensure: [ wrappers do: [ :each | each uninstall ] ] ] valueUnpreemptively.

	wrappers := wrappers reject: [:each | each hasRun].
	wrappers := wrappers collect: [:each | each reference].
	wrappers isEmpty 
		ifTrue:  [ UIManager default inform: 'Congratulations. Your tests cover all code under analysis.' ]
		ifFalse: [ 
			Smalltalk tools messageList 
				browse: wrappers
				title: 'Not Covered Code (' , (100 - (100 * wrappers size // methods size)) printString , '% Code Coverage)'].
	self saveResultInHistory