class initialization
initialize
	"This class should be initialized using the textual unicode definitions that can be found in
	http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
	http://www.unicode.org/Public/UNIDATA/CaseFolding.txt
	
	To do that, this class posesses the class side methods:
	#parseCaseMappingFrom:
	#parseUnicodeDataFrom:
	
	The initialization order is:
	1) tag constants
	2) unicode data
	3) case mappings
	"
	"If you are in a full image, you can try the following Zinc scripts to download the file automatically:
	
	UIManager default informUserDuring: [ :bar| | result |
            bar label: 'Downloading Unicode data'.
            (result := ZnEasy get: 'http://www.unicode.org/Public/UNIDATA/UnicodeData.txt') isSuccess
                ifFalse: [ ^ self error: 'Download failed' ].
            bar label: 'Updating Case Mappings'.
            self parseUnicodeDataFrom: result contents readStream ].

	UIManager default informUserDuring: [ :bar| | result |
		bar label: 'Downloading Unicode case mappings data'.
		(result := ZnEasy get: 'http://www.unicode.org/Public/UNIDATA/CaseFolding.txt') isSuccess
			ifFalse: [ ^ self error: 'Download failed' ].
		bar label: 'Updating Case Mappings'.
		self parseCaseMappingFrom: result contents readStream ].
	"
	
	EncodedCharSet declareEncodedCharSet: self atIndex: 0+1.
	EncodedCharSet declareEncodedCharSet: self atIndex: 256.
