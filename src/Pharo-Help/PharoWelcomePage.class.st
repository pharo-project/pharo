"
Pharo 5.0
=========

Welcome to Pharo, an immersive live programming environment.

""Pharo 5.0 already comes pre-loaded with a rich set of packages that you can use to explore the system and develop your own applications. However there is also a huge library of user contributed projects that you can also load using the 'Configurations Browser' by executing:""

MetacelloConfigurationBrowser open.

""This browser is also accessible from the World | Tools menu (just click the desktop, select Tools, and then Configuration Browser).

You can find information about Pharo on http://www.pharo.org. 

Some useful starting points are:

- Joining us and getting help http://www.pharo.org/get-help
- Getting the Pharo By Example book (available as a free PDF): http://www.pharobyexample.org
- Browser the documentation http://www.pharo.org/documentation
- Reporting problems http://www.pharo.org/get-involved

About this release
---------------------------
There are more than 1600 issues treated in this release. 

Sumarized issues for 4.0:
=========================

New stuff
---------

- GTools (Playground, Inspector, Spotter)
- Slots model instance variables as first class enities and enable meta-programming on this level.
- ShoreLine reporter: submit information automatically when errors happen
- TxModel, a modern text model who works with Athens (Preview)
- OSWindow, a new way to handle windows and input events (Preview)
- Glamour, a mature declarative browser builder
- Dark theme

Updated stuff
-------------

- Zinc/Zodiac
- Fuel
- Versionner

And a lot more!
You can check a more detailed explanation here: 
https://github.com/pharo-project/pharo-changelogs/blob/master/Pharo40ChangeLogs.md""
"
Class {
	#name : #PharoWelcomePage,
	#superclass : #Object,
	#category : 'Pharo-Help'
}

{ #category : #opening }
PharoWelcomePage class >> open [
	<script>
	| group welcome help zen about window |

	welcome := WelcomeHelp open.
	help := HelpBrowser open.

	zen := TextPresenter new
			text: ProfStef pharoZenValuesContents;
			title: 'Pharo Zen';
			aboutText: ProfStef aboutPharoZen;
			beForText;
			openWithSpec.
			
	about := TextPresenter new
			text: Smalltalk systemInformationString withCRs;
			title: 'About Pharo';
			beForText;
			openWithSpec.

	group := GroupWindowMorph new.
	group addWindow: welcome window.
	group addWindow: help window.
	group addWindow: SettingBrowser open.
	group addWindow: KeymapBrowser new openWithSpec window.
	group addWindow: zen window.
	group addWindow: about window.
	window := (group openInWindowLabeled: self title translated)
		extent: welcome window extent;
		yourself.
		
	group tabGroup selectedPageIndex: 1.

	^ window
]

{ #category : #opening }
PharoWelcomePage class >> openForRelease [
	<script>
	| window |
	
	World submorphs 
		detect: [ :each | (each isKindOf: SystemWindow) and: [ each label = self title ] ] 
		ifFound: [ :oldWindow | oldWindow delete ].
	window := self open.
	window extent: 750@400.
	window center: Display extent / 2
]

{ #category : #accessing }
PharoWelcomePage class >> title [
	^ 'Welcome to ', SystemVersion current version  
]
