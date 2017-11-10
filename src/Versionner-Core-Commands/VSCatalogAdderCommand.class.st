"
A VSCatalogAdderCommand adds methods to ConfigurationOf class to declare catalog metadata.


"
Class {
	#name : #VSCatalogAdderCommand,
	#superclass : #VSConfigurationCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #accessing }
VSCatalogAdderCommand >> configurationMetaclass [
	^ self configurationClass class
]

{ #category : #'metadata methods defintion' }
VSCatalogAdderCommand >> defineCatalogDescriptionTemplate [
	self configurationMetaclass
		compile: 'catalogDescription
	"Returns a paragraph describing the project"
	^ ''MyProject is an amazing project that will change your life.'''
		classified: 'catalog'
]

{ #category : #'metadata methods defintion' }
VSCatalogAdderCommand >> defineChangeLogTemplate [

	self configurationMetaclass 
		compile: 'catalogChangeLog
	"Returns a paragraph describing the most important changes in the configuration class."
	^ ''- Version number - Date - topics
[[[
	ConfigurationOfXXX project version: ''''xx'''' ) load					
]]]
or simply
- Version number - Date - topics
- Version number - Date - topics
- Version number - Date - topics.'''
		classified: 'catalog'
]

{ #category : #'metadata methods defintion' }
VSCatalogAdderCommand >> defineContactInfoTemplate [

	self configurationMetaclass 
		compile: 'catalogContactInfo
	"Returns a paragraph describing contact information such as email, mailing lists and website."
	^ ''MyProject is an amazing project . You can get in contact with our great MyProject programmer at myProject@gmail.com'''
		classified: 'catalog'
]

{ #category : #'metadata methods defintion' }
VSCatalogAdderCommand >> defineKeyClassesTemplate [

	self configurationMetaclass 
		compile: 'catalogKeyClassesAndExample
	"Returns a paragraph or more describing the key classes of your project. You can use Pillar/Pier syntax to layout out the text i.e., ==Code== and - for bullet."
	^ ''MyProject is composed of one cool key class named ==MyProject==. There is also ==MyKeyHelper==
- ==MyProject== is the responsible for many cool aspects.
- ==MyKeyHelper== is helping ==MyProject=='''
		classified: 'catalog'
]

{ #category : #'metadata methods defintion' }
VSCatalogAdderCommand >> defineKeywordsTemplate [

	self configurationMetaclass 
		compile: 'catalogKeywords
	"Returns an array of symbols"
	^ #(web fun) ' 
		classified: 'catalog'
]

{ #category : #execute }
VSCatalogAdderCommand >> ensureCatalogDescription [

	^ self hasCatalogDescription 
		ifFalse: [ self defineCatalogDescriptionTemplate ]
]

{ #category : #execute }
VSCatalogAdderCommand >> ensureChangeLog [
	^ self hasChangeLog 
		ifFalse: [ self defineChangeLogTemplate ]
]

{ #category : #execute }
VSCatalogAdderCommand >> ensureContactInfo [

	^ self hasContactInfo 
		ifFalse: [ self defineContactInfoTemplate ]
]

{ #category : #execute }
VSCatalogAdderCommand >> ensureKeyClasses [

	^ self hasKeyClasses
		ifFalse: [ self defineKeyClassesTemplate ]
]

{ #category : #execute }
VSCatalogAdderCommand >> ensureKeywords [

	^ self hasKeywords 
		ifFalse: [ self defineKeywordsTemplate ]
]

{ #category : #execute }
VSCatalogAdderCommand >> execute [
	self 
		ensureChangeLog;
		ensureCatalogDescription ;
		ensureContactInfo ;
		ensureKeyClasses ;
		ensureKeywords 
]

{ #category : #testing }
VSCatalogAdderCommand >> hasCatalogDescription [

	^ (self configurationClass includesSelector: #catalogDescription) 
]

{ #category : #testing }
VSCatalogAdderCommand >> hasChangeLog [

	^ (self configurationClass includesSelector: #catalogChangeLog) 
]

{ #category : #testing }
VSCatalogAdderCommand >> hasContactInfo [

	^ (self configurationClass includesSelector: #catalogContactInfo) 
]

{ #category : #testing }
VSCatalogAdderCommand >> hasKeyClasses [

	^ (self configurationClass includesSelector: #catalogKeyClassesAndExample) 
]

{ #category : #testing }
VSCatalogAdderCommand >> hasKeywords [

	^ (self configurationClass includesSelector: #catalogKeywords) 
]

{ #category : #accessing }
VSCatalogAdderCommand >> title [
	^ 'add catalog methods'
]
