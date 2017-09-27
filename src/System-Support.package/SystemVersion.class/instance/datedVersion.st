accessing-version strings
datedVersion
	" E.g. 'Pharo2.0a of ''18 April 2012''' "

	^ self version asString , ' of ' , self date printString.