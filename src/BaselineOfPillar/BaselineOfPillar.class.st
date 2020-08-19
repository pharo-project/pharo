Class {
	#name : #BaselineOfPillar,
	#superclass : #BaselineOf,
	#category : #BaselineOfPillar
}

{ #category : #baselines }
BaselineOfPillar >> baseline: spec [
	<baseline>

	spec
		for: #common
		do: [ spec blessing: #baseline.
			spec
				package: 'Pillar-Core';
				"Going from Pillar model to richText"
				package: 'Pillar-ExporterRichText' 
					with: [ spec requires: #('Pillar-Core') ] ]
]

{ #category : #baselines }
BaselineOfPillar >> customProjectAttributes [
    "Edit to return a collection of any custom attributes e.g. for conditional loading: Array with: #'Condition1' with: #'Condition2.
	For more information see: http://code.google.com/p/metacello/wiki/CustomProjectAttrributes "

	| attributes |
	attributes := OrderedCollection new.
	OSPlatform current isWindows ifTrue: [ attributes add: #windows ].
	OSPlatform current isUnix    ifTrue: [ attributes add: #unix ].
	OSPlatform current isMacOS   ifTrue: [ attributes add: #osx ].
	^ attributes asArray
]
