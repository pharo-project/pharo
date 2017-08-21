accessing
majorMinor: aSeparatingString
	"Return the major/minor version number of the form XaSeparatingStringY, without any 'alpha' or 'beta' or other suffix. If the receiver needs upgrading, do it. Remove the guard in 1.3."
	"SystemVersion current majorMinor: '-'"
	"SystemVersion current majorMinor: ''"

	^String streamContents: [:str |
		str 
			nextPutAll: self major printString; 
			nextPutAll: aSeparatingString;
			nextPutAll: self minor printString]