"
I am a mock version of the CatalogProvider. 
As the catalog provider goes to the network it produces flaky tests!!
So I will return the projects required for the tests
"
Class {
	#name : #MockCatalogProvider,
	#superclass : #CatalogProvider,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #accessing }
MockCatalogProvider class >> forTest [

	Default := self new
]

{ #category : #accessing }
MockCatalogProvider class >> reset [

	Default := nil
]

{ #category : #private }
MockCatalogProvider >> loadProjects [
	"I am a mock version I do not go to the network"

	^ { self projectNamed: 'JSON'.
	    self projectNamed: 'Artefact'.
		 self projectNamed: 'ThirdProject'}
]

{ #category : #private }
MockCatalogProvider >> projectNamed: aName [

	^ CatalogProject fromDictionary: { #name -> aName. #packageName -> aName.
	#repositoryUrl -> ('http://github.com/lala/' , aName) } asDictionary 
]
