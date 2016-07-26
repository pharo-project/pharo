class initialization
initializeNamedClassSelectors
	"self initializeNamedClassSelectors"

	(NamedClassSelectors := Dictionary new)
		at: 'alnum' put: #beAlphaNumeric;
		at: 'alpha' put: #beAlphabetic;
		at: 'cntrl' put: #beControl;
		at: 'digit' put: #beDigit;
		at: 'graph' put: #beGraphics;
		at: 'lower' put: #beLowercase;
		at: 'print' put: #bePrintable;
		at: 'punct' put: #bePunctuation;
		at: 'space' put: #beSpace;
		at: 'upper' put: #beUppercase;
		at: 'xdigit' put: #beHexDigit