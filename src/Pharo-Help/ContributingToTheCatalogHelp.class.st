"
Help file on how to contribute to catalog
"
Class {
	#name : #ContributingToTheCatalogHelp,
	#superclass : #CustomHelp,
	#category : #'Pharo-Help'
}

{ #category : #pages }
ContributingToTheCatalogHelp class >> addingYourProject [
	"This method was automatically generated."
	<script: 'self edit: #addingYourProject'>
	^HelpTopic
		title: 'Adding your Project'
		contents: 
'It is relatively easy to make your application easy to install from the Catalog. First of all, you need to define a configuration for your application. Configuration are a powerful mechanism to version your application.

If you do not know what a Configuration is, we advise you to have a look at DeepIntoPharo, a very cool book :-) about the advanced features of Pharo. The book is available online: http://deepintopharo.com
The book contains a chapter about Metacello. Metacello is the platform that handles configurations. You can also use Versionner, a UI to creates configurations.

Once you have your configuration, you need to defines meta informations under some particual methods name, located in the __CLASS SIDE__:
	- #catalogDescription  ==> A small paragraph describing your project.
	- #catalogContactInfo ==> Contact information such as email, mailing lists and website.
	- #catalogKeywords ==> An array of keys to better index your project

Also, please be sure to define a correct stable version and if needed a development version.
Finally, copy your configuration into one of the public repositories:

  MCSmalltalkhubRepository
	owner: ''Pharo''
	project: ''MetaRepoForPharo50''
	user: ''''
	password: ''''
	
The catalog list is refreshed each 24h - so by the next day latest your project will be available in 

 http://catalog.pharo.org and the Catalog Browser. 	
	!' readStream nextChunkText
]

{ #category : #accessing }
ContributingToTheCatalogHelp class >> bookName [
	^'Your app listed in the catalog'
]

{ #category : #pages }
ContributingToTheCatalogHelp class >> introduction [
	"This method was automatically generated."
	<script: 'self edit: #introduction'>
	^HelpTopic
		title: 'Introduction'
		contents: 
'The Pharo catalog is the list of registered Pharo projects. Such projects are easily installable using the Catalog Browser, accessible from the World menu, under the Tools menu entry.

It is also possible to access the catalog items directly from Pharo Spotter.

The catalog has a website, from which all the Pharo projects are available from:

	http://catalog.pharo.org


There is a machine readable JSON version available at:

    http://catalog.pharo.org/catalog/json	!' readStream nextChunkText
]

{ #category : #accessing }
ContributingToTheCatalogHelp class >> pages [
	^#(introduction addingYourProject)
]
