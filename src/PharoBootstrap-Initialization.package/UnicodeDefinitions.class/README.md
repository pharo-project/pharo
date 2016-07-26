I hold data needed to initialize the Unicode class. I'm used by the bootstrap process because the bootstrap does not yet have access to files nor network.

The data comes from the textual unicode definitions that can be found in
	http://www.unicode.org/Public/UNIDATA/UnicodeData.txt
	http://www.unicode.org/Public/UNIDATA/CaseFolding.txt

Initialization is done with:
	self initializeUnicodeClass