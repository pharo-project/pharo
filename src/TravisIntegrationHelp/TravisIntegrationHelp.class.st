"
Help for getting Travis running with Pharo.
"
Class {
	#name : #TravisIntegrationHelp,
	#superclass : #CustomHelp,
	#category : 'TravisIntegrationHelp'
}

{ #category : #accessing }
TravisIntegrationHelp class >> bookName [
	^ 'Travis Integration'
]

{ #category : #pages }
TravisIntegrationHelp class >> introduction [
	^ HelpTopic 
		title: 'Introduction to Travis Integration'
		contents: 
		(self heading: 'Introduction to Travis Integration'),
		'Travis is a wunderful continuous integration server. Thanks to SmalltalkCI, it can be easily used with any Pharo application.

The example used in this help is Roassal2. 

This help has been written as a cheatsheet and does not constitute a complete documentation.'
]

{ #category : #pages }
TravisIntegrationHelp class >> matrix [
	^ HelpTopic 
		title: 'Matrix Configuration'
		contents: 
		(self heading: 'Matrix Configuration'),	
		'It may happen that you need different configuration. For example, having a development configuration and a stable one. The development configuration usually consider the latest version of your packages, while the stable uses particular versions. 
		
You would need to create a new ', (self bold: '.smalltalk.ston'), ' file. To follow with our example, we have introduced this file in Step 04. We will now define a new file, called ', (self bold: '.smalltalk_stable.ston'), ' with the following content:
',
(self bold: '
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #configuration : ''Roassal2'',
      #repository : ''http://smalltalkhub.com/mc/ObjectProfile/Roassal2/main/'',
      #load : [ ''default'' ],
      #platforms : [ #pharo ],
      #version: #stable
    }
  ],
  
  #testing : {
  	#coverage : {
  	 #categories : [ ''Roassal2-AttachPoint-Tests'', ''Roassal2-Builder-AnimatedScatterPlot'', ''Roassal2-Builder-ApplicationMatrix'', ''Roassal2-Builder-CircularMap'', ''Roassal2-Builder-Common'', ''Roassal2-Builder-Composer'', ''Roassal2-Builder-DSM'', ''Roassal2-Builder-Explora'', ''Roassal2-Builder-Grapher-Decorators-Tests'', ''Roassal2-Builder-Grapher-Scaling-Tests'', ''Roassal2-Builder-Grapher-Tests'', ''Roassal2-Builder-Kiviat'', ''Roassal2-Builder-Legend'', ''Roassal2-Builder-Map-Tests'', ''Roassal2-Builder-Mondrian'', ''Roassal2-Builder-MondrianOLD'', ''Roassal2-Builder-NameCloud'', ''Roassal2-Builder-PieChart'', ''Roassal2-Builder-ScrollBar'', ''Roassal2-Builder-Simon'', ''Roassal2-Builder-Spectrograph'', ''Roassal2-Builder-StackBarPlot'', ''Roassal2-Builder-TextBuilder'', ''Roassal2-Builder-Timeline-Tests'', ''Roassal2-Builder-TreeMap'', ''Roassal2-Builder-UI-Slider'', ''Roassal2-Builder-UML'',  ''Roassal2-ColorNormalizer-Tests'', ''Roassal2-Core-Tests'', ''Roassal2-Exporter-SVG'', ''Roassal2-Interactions'', ''Roassal2-Exporter-PNG'', ''Roassal2-Exporter-HTML5'', ''Roassal2-Interactions-Resize-Tests'', ''Roassal2-Layouts-Tests'', 
''Roassal2-LineDecorations'', ''Roassal2-MorphSpecific'', ''Roassal2-OpenStreetMap'', ''Roassal2-Plugins'', ''Roassal2-Sparkline'', ''Roassal2-Table'', ''Roassal2-Tests'', ''Roassal2-Utility'', ''Trachel-MorphSpecific'', ''Trachel-Tests'', ''Trachel-Viva'', ''Roassal2GT-Glamour'', ''Roassal2GT-Glamour-Actions'', ''Roassal2GT-Glamour-Context'', ''Roassal2GT-Glamour-Roassal-Interaction'', ''Roassal2GT-Tests-Examples''
 ]
   	},
    
    #exclude : {
        #classes : [ #RTCacheTest , #Roassal2VWTest ]
    }
   }
}
'),
'

The principal difference between this file and ', (self bold: '.smalltalk.ston'), ' is the ', (self bold: ' #version: #stable'),
'

We are now ready to tell to Travis that we want to use this new file in some particular conditions, defined with a matrix. 
The ', (self bold: '.travis.yml'), ' should be:
',
(self bold: 
'language: smalltalk

os:
  - osx
  - linux

smalltalk:
  - Pharo-6.0
  - Pharo-5.0

matrix:
    include: 
        - smalltalk: Pharo-6.0
          smalltalk_config: .smalltalk_stable.ston
          os: osx
        - smalltalk: Pharo-5.0
          smalltalk_config: .smalltalk_stable.ston
          os: osx
        - smalltalk: Pharo-6.0
          smalltalk_config: .smalltalk_stable.ston
          os: linux
        - smalltalk: Pharo-5.0
          smalltalk_config: .smalltalk_stable.ston
          os: linux

'),
'

With this new content, Travis will run different configurations.'
]

{ #category : #pages }
TravisIntegrationHelp class >> moreInformation [
	^ HelpTopic 
		title: 'More information'
		contents:
		(self heading: 'More information'),
'Currently, there is no automatic way to start a build by simply committing to SmalltalkHub. The CronJob will run the test every 24 hours. You can manually run the build by either clicking on "start build" in Travis, or doing a commit in the git repository. 

Personally, I have created a short shell script, called update.sh, that simply does',
(self bold:	'git commit --allow-empty -m "Trigger" && git push'),
'

This help is written by Alexandre Bergel (http://bergel.eu) with the help of Juraj Kubelka. 

Big thanks to the HPI team that has produced the Pharo support. More information about the Pharo support on Travis may be found on: ',
(self url: 'https://github.com/hpi-swa/smalltalkCI')

]

{ #category : #accessing }
TravisIntegrationHelp class >> open [
	^ (HelpBrowser openOn: self) next
]

{ #category : #accessing }
TravisIntegrationHelp class >> pages [
	^ #(introduction step01 step02 step03 step04 step05 matrix moreInformation)
]

{ #category : #pages }
TravisIntegrationHelp class >> step01 [
	^ HelpTopic 
		title: 'Step 01 - Configuration'
		contents: 
		(self heading: 'Step 01 - Configuration'),
		'You need a configuration for your project. A configuration contains the necessary to load your application, and Travis has to know about it.
		
Before moving to the next step, you need to have a working configuration. Note that it does not matter where your configuration is located (e.g., in your SmalltalkHub repository, or in MetaRepoForPharoXX).

In this help, we will use the ConfigurationOfRoassal2 located in ', (self url: 'http://smalltalkhub.com/mc/Pharo/MetaRepoForPharo60/main')
]

{ #category : #pages }
TravisIntegrationHelp class >> step02 [
	^ HelpTopic 
		title: 'Step 02 - GitHub repository'
		contents: 
		(self heading: 'Step 02 - GitHub repository'),
		'You need a GitHub repository that will contains some configuration files. Even if your code is stored in SmalltalkHub, you need a GitHub repository.
		
You can have a look at the Roassal2 GitHub repository: https://github.com/ObjectProfile/Roassal2
'
]

{ #category : #pages }
TravisIntegrationHelp class >> step03 [
	^ HelpTopic 
		title: 'Step 03 - Account on Travis-ci.org'
		contents: 
		(self heading: 'Step 03 - Account on Travis-ci.org'),
		'* Go to http://travis-ci.org
* connect to it using your GitHub account
* You can add a repository (click on the + button on the left hand side, next to ''My Repositories''
* Select the repository you would like to have a build for. If you also be part of an organization you belong to. Move the switch to enable the repository
* Click on the repository to define a cron Jobs on the repository (you can configure it with Runs daily and always run)
'
]

{ #category : #pages }
TravisIntegrationHelp class >> step04 [
	^ HelpTopic 
		title: 'Step 04 - Define .smalltalk.ston'
		contents: 
		(self heading: 'Step 04 - Define .smalltalk.ston'),
		'In your Git repository you will need to add two files, .smalltalk.ston and .travis.yml (yes, with a period at the begining).
		
#configuration is the name of your project. Travis will look for ConfigurationOfRoassal2 in that case
#repository where the configuration is kept
#load is the Metacello Configuration group, usually defined in the baseline
#version 
#version corresponds to the name of a baseline 
#categories contains a list of class categories (also called tags) that contains unit tests.

-=-=-=-=-=
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #configuration : ''Roassal2'',
      #repository : ''http://smalltalkhub.com/mc/ObjectProfile/Roassal2/main/'',
      #load : [ ''default'' ],
      #platforms : [ #pharo ],
      #version: #development
    }
  ],

  #testing : {
   #categories : [ ''Roassal2-AttachPoint-Tests'', ''Roassal2-Builder-AnimatedScatterPlot'', ''Roassal2-Builder-ApplicationMatrix'', ''Roassal2-Builder-CircularMap'', ''Roassal2-Builder-Common'', ''Roassal2-Builder-Composer'', ''Roassal2-Builder-DSM'', ''Roassal2-Builder-Explora'', ''Roassal2-Builder-Grapher-Decorators-Tests'', ''Roassal2-Builder-Grapher-Scaling-Tests'', ''Roassal2-Builder-Grapher-Tests'', ''Roassal2-Builder-Kiviat'', ''Roassal2-Builder-Legend'', ''Roassal2-Builder-Map-Tests'', ''Roassal2-Builder-Mondrian'', ''Roassal2-Builder-MondrianOLD'', ''Roassal2-Builder-NameCloud'', ''Roassal2-Builder-PieChart'', ''Roassal2-Builder-ScrollBar'', ''Roassal2-Builder-Simon'', ''Roassal2-Builder-Spectrograph'', ''Roassal2-Builder-StackBarPlot'', ''Roassal2-Builder-TextBuilder'', ''Roassal2-Builder-Timeline-Tests'', ''Roassal2-Builder-TreeMap'', ''Roassal2-Builder-UI-Slider'', ''Roassal2-Builder-UML'',  ''Roassal2-ColorNormalizer-Tests'', ''Roassal2-Core-Tests'', ''Roassal2-Exporter-SVG'', ''Roassal2-Interactions'', ''Roassal2-Exporter-PNG'', ''Roassal2-Exporter-HTML5'', ''Roassal2-Interactions-Resize-Tests'', ''Roassal2-Layouts-Tests'', 
''Roassal2-LineDecorations'', ''Roassal2-MorphSpecific'', ''Roassal2-OpenStreetMap'', ''Roassal2-Plugins'', ''Roassal2-Sparkline'', ''Roassal2-Table'', ''Roassal2-Tests'', ''Roassal2-Utility'', ''Trachel-MorphSpecific'', ''Trachel-Tests'', ''Trachel-Viva'', ''Roassal2GT-Glamour'', ''Roassal2GT-Glamour-Actions'', ''Roassal2GT-Glamour-Context'', ''Roassal2GT-Glamour-Roassal-Interaction'', ''Roassal2GT-Tests-Examples''
 ]
   }
}
-=-=-=-=-=
'
]

{ #category : #pages }
TravisIntegrationHelp class >> step05 [
	^ HelpTopic 
		title: 'Step 05 - Define .travis.yml'
		contents: 
		(self heading: 'Step 05 - Define .travis.yml'),	
		'You need to indicates to Travis on which platform you want to run the tests. Consider the file:', String cr, String cr,
		(self bold: 
'language: smalltalk

os:
  - osx
  - linux

smalltalk:
  - Pharo-6.0
  - Pharo-5.0
'), '
This simple files indicates that the test of your application will be run over a matrix (osx, linux) X (Pharo-6.0, Pharo-5.0).

Travis will begin the build after you have pushed the .travis.yml and .smalltalk.ston files
'
]
