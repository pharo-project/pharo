"
Help for getting Travis running with Pharo.
"
Class {
	#name : #TravisIntegrationHelp,
	#superclass : #CustomHelp,
	#category : #'Pharo-Help'
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

The example used in this help is Roassal3. 

This help has been written as a cheatsheet and does not constitute a complete documentation.'
]

{ #category : #pages }
TravisIntegrationHelp class >> matrix [
	^ HelpTopic 
		title: 'Matrix Configuration'
		contents: 
		(self heading: 'Matrix Configuration'),	
		'It may happen that you need different configuration. For example, having a development configuration and a stable one. The development configuration usually consider the latest version of your packages, while the stable uses particular versions. 
		
You would need to create a new ', (self bold: '.smalltalk.ston'), ' file. To follow with our example, we have introduced this file in Step 04. We will now define a new file, called ', (self bold: '.smalltalk_pharo9.ston'), ' with the following content:
',
(self bold: '
SmalltalkCISpec {
  #preLoading: ''scripts/preLoading.st'',
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : ''Roassal3'',
      #directory : ''src'',
      #load : [ ''Core'', ''Tests'' ],
      #platforms : [ #pharo ]
    }
  ]
}
'),
'

The principal difference between this file and ', (self bold: '.smalltalk.ston'), ' is the ', (self bold: '#load : [ ''Core'', ''Tests'' ]'),
'

We are now ready to tell to Travis that we want to use this new file in some particular conditions, defined with a matrix. 
The ', (self bold: '.travis.yml'), ' should be:
',
(self bold: 
'language: smalltalk
sudo: false

os:
  - osx
  - linux

smalltalk:
  - Pharo64-8.0
  - Pharo64-9.0

matrix:
  fast_finish: true
  include: 
      - smalltalk: Pharo64-8.0
        smalltalk_config: .smalltalk.ston
        os: osx
      - smalltalk: Pharo64-9.0
        smalltalk_config: .smalltalk.ston
        os: osx
      - smalltalk: Pharo64-8.0
        smalltalk_config: .smalltalk_pharo9.ston
        os: osx
      - smalltalk: Pharo64-9.0
        smalltalk_config: .smalltalk_pharo9.ston
        os: osx
      - smalltalk: Pharo64-8.0
        smalltalk_config: .smalltalk.ston
        os: linux
      - smalltalk: Pharo64-9.0
        smalltalk_config: .smalltalk.ston
        os: linux
      - smalltalk: Pharo64-8.0
        smalltalk_config: .smalltalk_pharo9.ston
        os: linux
      - smalltalk: Pharo64-9.0
        smalltalk_config: .smalltalk_pharo9.ston
        os: linux
'),
'

With this new content, Travis will run different groups defined in the baseline.'
]

{ #category : #pages }
TravisIntegrationHelp class >> moreInformation [
	^ HelpTopic 
		title: 'More information'
		contents:
		(self heading: 'More information'),
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
	^ #(introduction step01 #step02 #step03 #step04 matrix moreInformation)
]

{ #category : #pages }
TravisIntegrationHelp class >> step01 [
	^ HelpTopic 
		title: 'Step 01 - Configuration'
		contents: 
		(self heading: 'Step 01 - Configuration'),
		'You need a configuration for your project. A configuration contains the necessary to load your application, and Travis has to know about it.
		
Before moving to the next step, you need to have a working baseline. In this help, we will use the BaselineOfRoassal3 located in ', (self url: 'https://github.com/ObjectProfile/Roassal3/tree/master/src/BaselineOfRoassal3'),
'

If you do not have a baseline, you may want to read the excellent description: 
', (self url: 'https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/Baselines.md')
]

{ #category : #pages }
TravisIntegrationHelp class >> step02 [
	^ HelpTopic 
		title: 'Step 02 - Account on Travis-ci.org'
		contents: 
		(self heading: 'Step 02 - Account on Travis-ci.org'),
		'* Go to http://travis-ci.com 
* connect to it using your GitHub account
* You can add a repository (click on the + button on the left hand side, next to ''My Repositories''
* Select the repository you would like to have a build for. If you also be part of an organization you belong to. Move the switch to enable the repository
'
]

{ #category : #pages }
TravisIntegrationHelp class >> step03 [
	^ HelpTopic 
		title: 'Step 03 - Define .smalltalk.ston'
		contents: 
		(self heading: 'Step 04 - Define .smalltalk.ston'),
		'In your Git repository you will need to add two files, .smalltalk.ston and .travis.yml (yes, with a period at the beginning).
		
#configuration is the name of your project. Travis will look for ConfigurationOfRoassal2 in that case
#repository where the configuration is kept
#load is the Metacello Configuration group, usually defined in the baseline
#version 
#version corresponds to the name of a baseline 
#categories contains a list of class categories (also called tags) that contains unit tests.

-=-=-=-=-=
SmalltalkCISpec {
  #preLoading: ''scripts/preLoading.st'',
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : ''Roassal3'',
      #directory : ''src'',
      #platforms : [ #pharo ]
    }
  ]
}
-=-=-=-=-=

The instruction #preLoading: is useful to execute a Pharo script _before_ installing the baseline. In the case of Roassal3, we use preLoading to actually remove some packages to not interfer with the loading of Roassal3. If you are curious, you can have a look at this script ', (self url: 'https://github.com/ObjectProfile/Roassal3/blob/master/scripts/preLoading.st'),

'

In general, you will probably not need a preLoading instruction. You can follow the template:

-=-=-=-=-=
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : ''YOURBASELINEPREFIX'',
      #directory : ''FOLDERWHERESOURCEISCONTAINED'', 
      #platforms : [ #pharo ]
    }
  ]
}
-=-=-=-=-=

'
]

{ #category : #pages }
TravisIntegrationHelp class >> step04 [
	^ HelpTopic 
		title: 'Step 04 - Define .travis.yml'
		contents: 
		(self heading: 'Step 04 - Define .travis.yml'),	
		'You need to indicates to Travis on which platform you want to run the tests. Consider the file:', String cr, String cr,
		(self bold: 
'language: smalltalk

os:
  - osx
  - linux

smalltalk:
  - Pharo64-8.0
  - Pharo64-9.0
'), '
This simple files indicates that the test of your application will be run over a matrix (osx, linux) X (Pharo64-8.0, Pharo64-9.0).

Travis will begin the build after you have committed and pushed the .travis.yml and .smalltalk.ston files in your repository.
'
]
