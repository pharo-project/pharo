examples
profileSourcesFileViewing
	"The big test to check that a TextArea is able to compose and ''quickly'' show
	 a big file content "

	"self profileSourcesFileViewing"

	| contents |
	
	contents := nil.
	
	self
	show: 'Wait a moment loading text, the sources file is huge' translated
	while: [ 
		contents := (FileStream readOnlyFileNamed: Smalltalk sourcesFile basename) contents
	].

	TimeProfiler spyAllOn: [ 
		RubEditingArea new
			beWrapped;
			width: 600;
			updateTextWith: contents;
			openInWorld  
	]