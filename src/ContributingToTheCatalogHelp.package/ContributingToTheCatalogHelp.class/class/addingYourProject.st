pages
addingYourProject
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