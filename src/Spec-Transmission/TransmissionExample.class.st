Class {
	#name : #TransmissionExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'packages',
		'classes',
		'protocols',
		'methods',
		'code'
	],
	#category : #'Spec-Transmission-Examples'
}

{ #category : #specs }
TransmissionExample class >> defaultSpec [

	^ SpecBoxLayout newVertical
		add: (SpecBoxLayout newHorizontal 
			add: #packages;
			add: #classes;
			add: #protocols;
			add: #methods);
		add: #code;
		yourself
]

{ #category : #showing }
TransmissionExample class >> open [ 
	<script>
	
	^ self new openWithSpec
]

{ #category : #initialization }
TransmissionExample >> classTemplateFor: aPackage [
	
	^ 'Object subclass: #NameOfSubclass
	slots: {}
	classVariables: {}
	package: ''', aPackage name,''' ' 
	
]

{ #category : #initialization }
TransmissionExample >> initializeWidgets [
	
	packages := self newList displayBlock: #name. 
	classes := self newList displayBlock: #name. 
	protocols := self newList displayBlock: [ :aPair | aPair value name ]. 
	methods := self newList displayBlock: #selector. 
	code := self newCode.

	packages transmitTo: classes transform: [ :aPackage | aPackage definedClasses asArray ].
	packages transmitTo: code transform: [ :aPackage | self classTemplateFor: aPackage ].
	
	classes 
		transmitTo: protocols 
		transform: [ :aClass |
			aClass organization allProtocols 
				collect: [ :each | aClass -> each ]
				as: OrderedCollection ]
		postTransmission: [ :destination :origin | destination selectIndex: 1 ].
	classes transmitTo: code transform: #definitionForNautilusWithSlots.
			
	protocols 
		transmitTo: methods 
		transform: [ :aPair | 
			aPair value methods 
				collect: [ :each | aPair key >> each ]
				as: OrderedCollection ].
	protocols transmitTo: code transform: [ :aPair | aPair key sourceCodeTemplate ].

	methods transmitTo: code transform: #sourceCode.
	
	packages items: RPackageOrganizer default packages
]

{ #category : #initialization }
TransmissionExample >> initializeWindow: aWindowPresenter [

	aWindowPresenter
		title: 'Transmission example browser';
		initialExtent: 800@600
]
