"
I am the model for the `Finder` tool.

I am responsible for holding the search environment, executing searches and
returning search results.

For searching I use the interface defined by ̀FinderSearch̀ and all searches
should therefor be subclasses of ̀FinderSearch̀.

"
Class {
	#name : #FinderModel,
	#superclass : #Model,
	#instVars : [
		'searchString',
		'resultsDictionary',
		'useRegex',
		'environment',
		'searchEnvironment',
		'availableSearches',
		'currentSearch'
	],
	#category : #'Tool-Finder-Base'
}

{ #category : #'default values' }
FinderModel class >> defaultSearches [
	"Returns a list of supported searches. Each item in the list is a subclass of `FinderSearch`."

	"The first element is used as the default search mode."

	^ {
		  FinderSelectorSearch.
		  FinderClassSearch }
]

{ #category : #information }
FinderModel >> availablePackages [

	"Returns for searching available packages by name."

	^ environment packages
		  collect: [ :package | package name ]
		  as: OrderedCollection
]

{ #category : #accessing }
FinderModel >> availableSearches [

	"Returns the searches available to the model. The first one is used as the default search mode."

	^ availableSearches
]

{ #category : #accessing }
FinderModel >> currentSearch [

	"Returns the current search used by the model."

	^ currentSearch
]

{ #category : #accessing }
FinderModel >> currentSearch: aSearch [

	"Sets the search used by the model to aSearch. aSearch should be one of the availableSearches."

	currentSearch := aSearch
]

{ #category : #initialization }
FinderModel >> initialize [

	super initialize.
	searchString := ''.
	resultsDictionary := Dictionary new.
	useRegex := false.

	availableSearches := self class defaultSearches collect: [ :search |
		                     search new ].
	currentSearch := availableSearches first.

	environment := RBBrowserEnvironment new.
	self selectAllPackages
]

{ #category : #accessing }
FinderModel >> resultsDictionary [

	^ resultsDictionary
]

{ #category : #searching }
FinderModel >> search [
	"Performs a search and stores the results in `self resultsDictionary`."

	searchString isEmpty ifTrue: [ ^ self ].

	[ :job |
	job title: 'Searching...' translated.
	resultsDictionary := useRegex
		                     ifTrue: [
		                     currentSearch searchByRegex:
			                     searchString asRegex in: environment ]
		                     ifFalse: [
		                     currentSearch searchByString: searchString in: environment ].
	self valueChanged ] asJob run
]

{ #category : #searching }
FinderModel >> searchBy: aString [

	"Perform a search by aString."

	self searchString: aString.
	self search
]

{ #category : #accessing }
FinderModel >> searchString [

	"Return the current search string."

	^ searchString
]

{ #category : #accessing }
FinderModel >> searchString: aString [

	"Set the search string, which is to be used for further searches."

	searchString := aString
]

{ #category : #selection }
FinderModel >> selectAllPackages [

	"Selects all packages for searching."

	searchEnvironment := environment forPackages: environment packages
]

{ #category : #information }
FinderModel >> selectedPackages [

	"Returns for searching selected packages by name."

	^ searchEnvironment packageNames asOrderedCollection
]

{ #category : #selection }
FinderModel >> selectedPackagesByName: aCollection [

	"Selects all packages given in aCollection by their names for searching."

	searchEnvironment := environment forPackageNames: aCollection
]

{ #category : #information }
FinderModel >> unselectedPackages [

	"Returns for searching unselected packages by name."

	^ self availablePackages removeAllFoundIn: self selectedPackages
]

{ #category : #accessing }
FinderModel >> useRegex [

	^ useRegex
]

{ #category : #accessing }
FinderModel >> useRegex: aBoolean [

	useRegex := aBoolean
]
