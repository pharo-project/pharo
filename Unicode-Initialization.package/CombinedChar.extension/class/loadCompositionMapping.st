*Unicode-Initialization
loadCompositionMapping
	| url |
	url := 'http://unicode.org/Public/UNIDATA/UnicodeData.txt' asZnUrl.
	self parseCompositionMappingFrom: url retrieveContents readStream