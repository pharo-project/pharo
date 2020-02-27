Class {
	#name : #SpTransmissionExample,
	#superclass : #SpPresenter,
	#instVars : [
		'packages',
		'classes',
		'protocols',
		'methods',
		'code'
	],
	#category : #'Spec2-Transmission-Examples'
}

{ #category : #specs }
SpTransmissionExample class >> defaultSpec [

	^ SpBoxLayout newVertical
		add: (SpBoxLayout newHorizontal 
			add: #packages;
			add: #classes;
			add: #protocols;
			add: #methods);
		add: #code;
		yourself
]

{ #category : #showing }
SpTransmissionExample class >> open [ 
	<script>
	
	^ self new openWithSpec
]

{ #category : #initialization }
SpTransmissionExample >> classTemplateFor: aPackage [
	
	^ 'Object subclass: #NameOfSubclass
	slots: {}
	classVariables: {}
	package: ''', aPackage name,''' ' 
	
]

{ #category : #initialization }
SpTransmissionExample >> initializePresenters [
	packages := self newList display: #name.
	classes := self newList display: #name.
	protocols := self newList display: [ :aPair | aPair value name ].
	methods := self newList display: #selector.
	code := self newCode.
	packages
		transmitTo: classes
		transform: [ :aPackage | aPackage definedClasses asArray ].
	packages
		transmitTo: code
		transform: [ :aPackage | self classTemplateFor: aPackage ].
	classes
		transmitTo: protocols
		transform: [ :aClass | 
			aClass organization allProtocols
				collect: [ :each | aClass -> each ]
				as: OrderedCollection ]
		postTransmission: [ :destination :origin | destination selectIndex: 1 ].
	classes transmitTo: code transform: #definitionWithSlots.
	protocols
		transmitTo: methods
		transform: [ :aPair | 
			aPair value methods
				collect: [ :each | aPair key >> each ]
				as: OrderedCollection ].
	protocols
		transmitTo: code
		transform: [ :aPair | aPair key sourceCodeTemplate ].
	methods transmitTo: code transform: #sourceCode.
	packages items: RPackageOrganizer default packages
]

{ #category : #initialization }
SpTransmissionExample >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: 'Transmission example browser';
		initialExtent: 800@600
]
