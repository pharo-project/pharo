api
import: aString provides: aCollection
  "import names defined in baseline <aString> to be used when loading the version

	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' provides: #('Sample Core');
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: 'Example' provides: #('Example Core')]	
	 "

  self root import: aString provides: aCollection constructor: self