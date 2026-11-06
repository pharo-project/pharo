Class {
	#name : 'ReflectivityTestCase',
	#superclass : 'TestCase',
	#instVars : [
		'registeredLinksBefore'
	],
	#category : 'Reflectivity-Tests-Base',
	#package : 'Reflectivity-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ReflectivityTestCase >> setUp [
	"Remember the links already registered to objects, so the test only uninstalls its own links"

	super setUp.
	registeredLinksBefore := MetaLink linkInstaller linksRegistry objectSpecificLinks
]

{ #category : 'running' }
ReflectivityTestCase >> tearDown [
	"Uninstall the links this test registered to objects, keeping the links that were already there before the test"

	MetaLink linkInstaller linksRegistry objectSpecificLinks
		reject: [ :link | registeredLinksBefore includes: link ]
		thenDo: [ :link | link uninstall ].
	super tearDown
]
