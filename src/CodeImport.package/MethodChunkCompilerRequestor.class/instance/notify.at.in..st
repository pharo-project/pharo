interactive error protocol
notify: message at: location in: code
	| newSource |
	self flag: #hack.
	"Should use the new OPalWarnings directly instead of recreating a SyntaxErrorNotification"
	newSource := SyntaxErrorNotification 
		inClass: STCommandLineHandler
		withCode: code
		doitFlag: false
		errorMessage: message
		location: location.
		
	"If the syntax error notification is resumed, then the source was corrected and we have to announce that parsing can restart."					
	ReparseAfterSourceEditing signalWithNewSource: newSource