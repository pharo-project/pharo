editing
accept: aSelector title: title contents: text
	"Accept edited text. Compile it into a HelpTopic"

	| code |
	code := String streamContents:[:s|
		s nextPutAll: aSelector.
		s crtab; nextPutAll: '"This method was automatically generated."'.
		s crtab; nextPutAll: '<script: ''self edit: ', aSelector storeString,'''>'.
		s crtab; nextPutAll: '^HelpTopic'.
		s crtab: 2; nextPutAll: 'title: ', title storeString.
		s crtab: 2; nextPutAll: 'contents: '.
		s cr; nextPutAll: (String streamContents:[:c| c nextChunkPut: text]) storeString.
		s nextPutAll:' readStream nextChunkText'.
	].

	self class 
		compile: code
		classified: ((self class organization categoryOfElement: aSelector) ifNil:['pages']).
