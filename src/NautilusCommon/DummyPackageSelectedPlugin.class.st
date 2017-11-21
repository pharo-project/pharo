"
A DummyPackageSelectedPlugin is a dummy plugin which displays the selected package
"
Class {
	#name : #DummyPackageSelectedPlugin,
	#superclass : #AbstractPackageSelectedPlugin,
	#instVars : [
		'morph'
	],
	#category : #NautilusCommon-Plugin-Examples
}

{ #category : #position }
DummyPackageSelectedPlugin class >> defaultPosition [

	^ #middle
]

{ #category : #display }
DummyPackageSelectedPlugin >> display [

	morph :=  LabelMorph new 
					contents: '';
					enabled: false;
					vResizing: #shrinkWrap;
					hResizing: #spaceFill;
					yourself.
	^ morph
]

{ #category : #announcement }
DummyPackageSelectedPlugin >> packageSelected: anAnnouncement [
	
	| package name |
	package := anAnnouncement package.
	name := package name asString.
	package ifNil: [ name := ''].
	morph ifNotNil: [ morph contents: name]
]
