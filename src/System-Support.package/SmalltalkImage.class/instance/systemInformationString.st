sources, change log
systemInformationString
	^ String cr, 'Pharo ', SystemVersion current dottedMajorMinor,  String cr, self lastUpdateString, String cr, String cr, String cr, self licenseString