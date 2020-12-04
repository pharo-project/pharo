"
This baseline is used to load the basic support needed for remote image browsing with TelePharo. It is not used during the building of the standard image.

Load this baseline:

./pharo minimal.image eval --save ""Metacello new baseline: 'TelePharoSupport';repository: 'github://pharo-project/pharo:Pharo9.0/src'; load""

Load the TelePharo 

./pharo minimal.image eval --save ""Metacello new repository: 'github://pharo-ide/TelePharo'; baseline: 'TelePharo'; load: #( 'MinimalServer' )""

For more information and prepared scripts see: 
   https://github.com/noha/pharo-minimal
"
Class {
	#name : #BaselineOfTelePharoSupport,
	#superclass : #BaselineOf,
	#category : #BaselineOfTelePharoSupport
}

{ #category : #baseline }
BaselineOfTelePharoSupport >> baseline: spec [ 
    
	<baseline>

	| repository | 
	
	repository := self packageRepositoryURL.

	spec for: #common do: [
		spec baseline: 'SUnit' with: [spec repository: repository].
		spec baseline: 'Display' with: [spec repository: repository].

		spec package: 'SortFunctions-Core'.
		spec package: 'Tool-Registry'.
	].
]
