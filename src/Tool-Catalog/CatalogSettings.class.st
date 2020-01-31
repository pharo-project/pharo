"
Settings for catalog
"
Class {
	#name : #CatalogSettings,
	#superclass : #Object,
	#classVars : [
		'CatalogSourceURL',
		'DisplayCatalogProjectsInSpotter'
	],
	#category : #'Tool-Catalog-Settings'
}

{ #category : #settings }
CatalogSettings class >> catalogSettingsOn: aBuilder [  
	<systemsettings>  
	(aBuilder group: #catalog)
		label: 'Catalog';
		parent: #tools;
		description: 'All Catalog and CatalogBrowser settings';
		with: [ 
			(aBuilder setting: #displayCatalogProjectsInSpotter)
						target: self;
						label: 'Display catalog projects in Spotter';
						default: false.
			(aBuilder setting: #catalogSourceURL)
				target: self;
				label: 'Source URL' ;
				default: 'http://catalog.pharo.org/catalog/json';
				ghostHelp: 'http://pathTo.url/json';
				description: 'Enter the source URL used to retrieve catalog information'			
		]
]

{ #category : #accessing }
CatalogSettings class >> catalogSourceURL [
	"Return the source URL for retrieving catalog infos"
	
	^ CatalogSourceURL ifNil: [ CatalogSourceURL := self defaultCatalogSourceURL ]
]

{ #category : #accessing }
CatalogSettings class >> catalogSourceURL: aString [
	"Set the source URL for retrieving catalog infos"
	
	CatalogSourceURL := aString
]

{ #category : #defaults }
CatalogSettings class >> defaultCatalogSourceURL [

	^'http://catalog.pharo.org/catalog/json'
]

{ #category : #accessing }
CatalogSettings class >> displayCatalogProjectsInSpotter [
	"Return true if the catalog projects should be displayed in spotter, false otherwise"
	
	^ DisplayCatalogProjectsInSpotter ifNil: [ DisplayCatalogProjectsInSpotter := true ]
]

{ #category : #accessing }
CatalogSettings class >> displayCatalogProjectsInSpotter: aBoolean [
	"Define if the catalog projects should be displayed in Spotter"
	
	DisplayCatalogProjectsInSpotter := aBoolean 
]
