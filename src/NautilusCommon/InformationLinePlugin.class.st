"
A little line to show information....
"
Class {
	#name : #InformationLinePlugin,
	#superclass : #AbstractNautilusPlugin,
	#instVars : [
		'morph'
	],
	#category : #NautilusCommon-Plugin
}

{ #category : #position }
InformationLinePlugin class >> defaultPosition [

	^ #middle
]

{ #category : #information }
InformationLinePlugin class >> description [

	^ 'Display the URI of the selected element'
]

{ #category : #private }
InformationLinePlugin >> buildString [
	^ ''
]

{ #category : #announcement }
InformationLinePlugin >> changed: ann [
	
	| string |
	string := self buildString.
	morph ifNotNil: [ morph contents: string ]
]

{ #category : #display }
InformationLinePlugin >> display [

	morph :=  LabelMorph new 
					contents: '';
					enabled: false;
					vResizing: #shrinkWrap;
					hResizing: #spaceFill;
					yourself.
	^ morph
]

{ #category : #registration }
InformationLinePlugin >> registerTo: aModel [

	aModel announcer
		when: NautilusPackageSelected send: #changed: to: self;
		when: NautilusClassSelected send: #changed: to: self;
		when: NautilusGroupSelected send: #changed: to: self;
		when: NautilusProtocolSelected send: #changed: to: self;
		when: NautilusMethodSelected send: #changed: to: self
]
