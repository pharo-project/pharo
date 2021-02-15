"
Provide access to icons from the system. 
It receive locations (directory references) where to find them. 
Alternatively, it can also look for gtk theme icons (stored by name in the gtk theme).
"
Class {
	#name : #SpLocationIconProvider,
	#superclass : #SpIconProvider,
	#instVars : [
		'icons',
		'locations'
	],
	#category : #'Spec2-Core-IconProvider'
}

{ #category : #'instance creation' }
SpLocationIconProvider class >> newLocation: aLocation [

	^ self new 
		addLocation: aLocation;
		yourself
]

{ #category : #accessing }
SpLocationIconProvider >> addLocation: aReference [

	locations := locations copyWith: aReference
]

{ #category : #accessing }
SpLocationIconProvider >> iconNamed: aName ifAbsent: aBlock [
	| icon |

	icon := icons at: aName ifAbsent: [ nil ].
	icon ifNil: [ 
		icon := self loadIconNamed: aName.
		icon ifNil: [ ^ aBlock value ].
		icons at: aName put: icon ].

	^ icon
]

{ #category : #accessing }
SpLocationIconProvider >> icons [

	^ icons
]

{ #category : #initialization }
SpLocationIconProvider >> initialize [ 

	super initialize.
	locations := #().
	icons := Dictionary new.
	icons at: #blank put: (Form extent: 16@16 depth: 8)
]

{ #category : #inspecting }
SpLocationIconProvider >> inspectionIcons [
	<inspectorPresentationOrder: 0 title: 'Icons'> 

	^ SpListPresenter new
		items: (icons associations sorted: [ :a :b | a key < b key ]);
		icon: [ :each | each value ];
		display: [ :each | each key ];
		yourself
]

{ #category : #utils }
SpLocationIconProvider >> loadAllIcons [

	self locations do: [ :eachLocation |
		(eachLocation filesMatching: '*.png') do: [ :each |
			icons 
				at: each basename
				put: (self loadIcon: each) ] ]
]

{ #category : #utils }
SpLocationIconProvider >> loadIcon: fileRef [

	fileRef exists ifFalse: [ 
		self error: ('Icon {1} does not exit' format: { fileRef fullName }) ].

	^ fileRef binaryReadStreamDo: [ :stream |
		 Form fromBinaryStream: stream ]
]

{ #category : #utils }
SpLocationIconProvider >> loadIconNamed: aSymbol [

	self locations do: [ :each | | ref |
		ref := (each / aSymbol) withExtension: 'png'.
		ref exists ifTrue: [ 
			^ self loadIcon: ref ] ].
	^ nil
]

{ #category : #accessing }
SpLocationIconProvider >> locations [

	^ locations
]

{ #category : #accessing }
SpLocationIconProvider >> menuIconNamed: aName [ 

	^ self iconNamed: aName
]
