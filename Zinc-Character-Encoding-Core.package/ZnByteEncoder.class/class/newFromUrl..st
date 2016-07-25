instance creation
newFromUrl: url
	"Instanciate a new encoder directly from a Unicode.org url"
	"self newFromUrl: 'http://unicode.org/Public/MAPPINGS/ISO8859/8859-2.TXT'"
	"self newFromUrl: 'http://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT'"

	| mapping spec tables |
	mapping := self parseUnicodeOrgSpec: url.
	spec := self top128FromUnicodeSpec: mapping.
	tables := self tablesFromSpec: spec.
	^ self new
		identifier: url;
		byteToUnicode: tables first;
		unicodeToByte: tables second;
		yourself