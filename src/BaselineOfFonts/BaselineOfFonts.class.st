Class {
	#name : 'BaselineOfFonts',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfFonts',
	#package : 'BaselineOfFonts'
}

{ #category : 'accessing' }
BaselineOfFonts class >> corePackageNames [

	^ self packagesOfGroupNamed: #Core
]

{ #category : 'baselines' }
BaselineOfFonts >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [ "Fonts draw on forms"
		spec baseline: 'Display' with: [ spec repository: repository ].

		spec
			package: 'Fonts-Abstract';
			package: 'Fonts-Infrastructure' with: [ spec requires: #( 'Fonts-Abstract' 'Text-Core' ) ];
			package: 'Fonts-Infrastructure-Tests' with: [ spec requires: #( 'Fonts-Infrastructure' ) ];
			package: 'Text-Core' with: [ spec requires: #( 'Fonts-Abstract' 'Display' ) ];
			package: 'Graphics-Fonts' with: [
				spec
					requires: #( 'Fonts-Abstract' 'Fonts-Infrastructure' 'Text-Core' 'Display' );
					postLoadDoIt: #installDefaultFont:package: ].

		spec
			group: 'Core' with: #( 'Fonts-Abstract' 'Fonts-Infrastructure' 'Graphics-Fonts' 'Text-Core' );
			group: 'Tests' with: #( 'Fonts-Infrastructure-Tests' ) ]
]

{ #category : 'actions' }
BaselineOfFonts >> installDefaultFont: loader package: packageSpec [
	"Text needs a default font. Install the bitmap fonts kept in resources/fonts, next to the folder this baseline was loaded from, unless the image has a default font.
	StrikeFont exists only once Graphics-Fonts is loaded, so it is looked up"

	(self class environment at: #StrikeFont) installDefaultBitmapFontFrom: self sourceDirectory parent / 'resources' / 'fonts'
]

{ #category : 'accessing' }
BaselineOfFonts >> sourceDirectory [
	"The folder this baseline was loaded from, such as the repository's src folder"

	^ (self class package mcWorkingCopy repositoryGroup repositories detect: [ :each |
		   each ~= MCCacheRepository uniqueInstance and: [ each isKindOf: MCDirectoryRepository ] ]) directory asFileReference
]
