"
I am an abstract base class for searches in the `Finder` Tool.

I provide a common interface for performing searches and results are expected to be returned in
form of a `FinderResult` composite object.

# Subclass Responsibilities

The following methods must be provided by subclasses of me.

- **_name_** returns the name of the search
- **_searchByString:in:_** performs a search for given search string.
- **_searchByRegex:in:_** performs a search for a given `RxMatcher` object.


"
Class {
	#name : #FinderSearch,
	#superclass : #Object,
	#category : #'Tool-Finder-Search'
}

{ #category : #information }
FinderSearch >> name [

	"Returns the name of the search."

	self subclassResponsibility
]

{ #category : #searching }
FinderSearch >> searchByRegex: aRegex in: anEnvironment [
	"Perform a search given aRegex in anEnvironment."

	self subclassResponsibility
]

{ #category : #searching }
FinderSearch >> searchByString: aString in: anEnvironment [
	"Perform a search given aString in anEnvironment."

	self subclassResponsibility
]
