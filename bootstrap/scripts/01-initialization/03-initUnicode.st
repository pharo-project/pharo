
| dataFilePath caseFolding unicodeData|

unicodeData := CommandLineArguments new arguments last , 'UnicodeData.txt'.
caseFolding := CommandLineArguments new arguments last , 'CaseFolding-8.0.0.txt'.

EncodedCharSet initialize.

Unicode initializeTagConstants.

Unicode 
	parseUnicodeDataFrom: (ZnCharacterReadStream on: (File named: unicodeData) readStream encoding: 'utf8');
	parseCaseMappingFrom: (ZnCharacterReadStream on: (File named: caseFolding) readStream encoding: 'utf8').

Character characterSet: Unicode.

ByteTextConverter initialize.

CombinedChar parseCompositionMappingFrom: (ZnCharacterReadStream on: (File named: unicodeData) readStream encoding: 'utf8').