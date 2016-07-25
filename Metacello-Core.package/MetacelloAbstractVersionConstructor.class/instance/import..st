api
import: aStringOrCollection
  "import names defined in baseline <aString> to be used when loading the version

		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec import: 'Sample' ]
	
	or a list of names for multiple project imports:
	
		spec baseline: 'Sample' with: [ spec repository: 'github://dalehenrich/sample:master/repository' ].
		spec baseline: 'Example' with: [ spec repository: 'github://dalehenrich/example:master/repository' ].
		spec import: #('Sample' 'Example')]	
	 "

  self root import: aStringOrCollection constructor: self