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
	#category : #'Spec-Examples'
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
TransmissionExample >> initializeWidgets [
	
	packages := self newList displayBlock: #name. 
	classes := self newList displayBlock: #name. 
	protocols := self newList displayBlock: [ :aPair | aPair value name ]. 
	methods := self newList displayBlock: #selector. 
	code := self newText beForCode.

	self transmit from: packages; to: classes; transform: #definedClasses.
	self transmit from: classes; to: protocols; transform: [ :aClass | 
		aClass organization allProtocols collect: [ :each | aClass -> each ] ].
	self transmit from: protocols; to: methods; transform: [ :aPair | 
		aPair value methods collect: [ :each | aPair key >> each ] ].
	self transmit from: methods; to: code; transform: #sourceCode.
	
	packages items: RPackageOrganizer default packages
]
