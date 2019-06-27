"
I represent an abstract inspection element. In an EyeInspector, a eye element corresponds to a wrapper around a field element.

Public API :
label: display on left list panel of the eye inspector
description: printed on the right panel of the eye inspector
host: object inspected in the eye inspector 
value: object represented by this eye element 
"
Class {
	#name : #SpAbstractEyeElement,
	#superclass : #Model,
	#traits : 'TSpInspectorActions',
	#classTraits : 'TSpInspectorActions classTrait',
	#instVars : [
		'host'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
SpAbstractEyeElement class >> host: anObject [ 
	^ self new
		host: anObject;
		yourself
]

{ #category : #comparing }
SpAbstractEyeElement >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ host == anObject host
]

{ #category : #accessing }
SpAbstractEyeElement >> accessorCode [
	"Answers a code string to access the value from the inspector"
	self subclassResponsibility
]

{ #category : #menu }
SpAbstractEyeElement >> browseSubMenu: aMenu [
	
	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Browse Full';
				action: [ self browseSelectedObject ];
				shortcut: PharoShortcuts current browseShortcut  ].
			
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Browse Class';
				action: [ self browseSelectedObjectClass ] ].
			
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Browse Hierarchy';
				action: [ self browseSelectedObjectClassHierarchy ];
				shortcut: $h meta ] ]
]

{ #category : #action }
SpAbstractEyeElement >> browseValue [
	^ self value browse
]

{ #category : #action }
SpAbstractEyeElement >> browseValueClass [
	"Open an class browser on the selectObject (class side)"
	
	^ self valueClass browse
]

{ #category : #action }
SpAbstractEyeElement >> browseValueClassHierarchy [
	"Create and schedule a class list browser on the receiver's hierarchy."

	^ self valueClass browseHierarchy
]

{ #category : #action }
SpAbstractEyeElement >> copyAccessorCode [
	"Copy the name of the current variable, so the user can paste it into the 
	window below and work with is."

	Clipboard clipboardText: self accessorCode asText
]

{ #category : #menu }
SpAbstractEyeElement >> customSubMenu: aMenu [
	"Subclasse may add more menu items here"
]

{ #category : #accessing }
SpAbstractEyeElement >> description [
	^ self value printStringLimitedTo: 2000
]

{ #category : #menu }
SpAbstractEyeElement >> endSubMenu: aMenu [ 
	
	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Copy Name';
				action: [ self copyAccessorCode ];
				shortcut: $c meta ] ]
]

{ #category : #accessing }
SpAbstractEyeElement >> errorPropertySelectors [
	"Return a collection of selectors on this eye-element that are checked against errors.
	See #withErrorsDo: and #errorSubMenu:"
	^ #(icon label longLabel description)
]

{ #category : #menu }
SpAbstractEyeElement >> errorSubMenu: aMenu [
	"Add debug menu entries for the failing #errorPropertySelectors of this eye element"
	
	self hasError 
		ifFalse: [ ^ self ].
		
	aMenu addGroup: [ :aGroup |
		
		self withErrorsDo: [ :accessError :itemSelector |
			aGroup addItem: [ :anItem |
				anItem 
					name: 'Debug Accessing ', itemSelector printString, ' of ', self accessorCode ;
					icon: (self iconNamed: #smallWarningIcon);
					action: [ [ self perform: itemSelector] fork ]]]]
]

{ #category : #accessing }
SpAbstractEyeElement >> errorWhileAccessing: selector do: aBlock [
	[ self perform: selector ]
		on: Error do: [ :error |  
			aBlock cull: error cull: selector ].
]

{ #category : #action }
SpAbstractEyeElement >> explorePointers [
	^ Smalltalk tools pointerExplorer openStrongOn: self value
]

{ #category : #menu }
SpAbstractEyeElement >> exploreSubMenu: aMenu [

	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Explore Pointers';
				action: [ self explorePointers ] ] ]
]

{ #category : #action }
SpAbstractEyeElement >> exploreValue [
	^ self value inspect
]

{ #category : #testing }
SpAbstractEyeElement >> hasError [
	self withErrorsDo: [ ^ true ].
	^ false
]

{ #category : #testing }
SpAbstractEyeElement >> hasSelectedObject [
	^ true
]

{ #category : #comparing }
SpAbstractEyeElement >> hash [
	^ host hash
]

{ #category : #accessing }
SpAbstractEyeElement >> host [
	"Answers the object currently inspected by the outer inspector.
	See #value for the object represented by this EyeElement."
	^ host
]

{ #category : #accessing }
SpAbstractEyeElement >> host: anObject [
	host := anObject
]

{ #category : #accessing }
SpAbstractEyeElement >> hostClass [
	^ host class
]

{ #category : #accessing }
SpAbstractEyeElement >> icon [
	^ (self value iconOrThumbnailOfSize: 16)
		ifNil: [ self value class systemIcon ]
]

{ #category : #action }
SpAbstractEyeElement >> inspectValue [
	"Bring up a non-special inspector"
	^ self value inspect
]

{ #category : #menu }
SpAbstractEyeElement >> inspectionMenu: aMenu [
	"specific menu for the current element"
		
	self 
		exploreSubMenu: aMenu;
		browseSubMenu: aMenu;
		customSubMenu: aMenu;
		endSubMenu: aMenu;
		errorSubMenu: aMenu.
]

{ #category : #accessing }
SpAbstractEyeElement >> label [
	"Answers the label associated with this EyeElement"
	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractEyeElement >> longLabel [
	"This is used by EyeTreeInspector"
	
	| description |
	description := self description.
	description size > 150
		ifTrue: [ description := description first: 150 ].
	^ self label 
		ifNotNil: [ :label | label , ': ' , description ] 
		ifNil: [ description ]
]

{ #category : #menu }
SpAbstractEyeElement >> mainInspectSubMenu: aMenu [ 
	aMenu 
		add: 'Inspect (i)' 
		target: self
		selector: #inspectSelectedObjectInNewWindow.
				
	aMenu
		add: 'Explore (I)' 
		target: self
		selector: #exploreSelectedObject.
]

{ #category : #printing }
SpAbstractEyeElement >> printOn: aStream [
	aStream
		print: self label;
		<< '->';
		<< (self value printStringLimitedTo: 50)
]

{ #category : #action }
SpAbstractEyeElement >> save: aValue [
	"Subclasse may override this"
]

{ #category : #accessing }
SpAbstractEyeElement >> selectedObject [
	^ self value
]

{ #category : #accessing }
SpAbstractEyeElement >> selectedObjectDo: aBlock [
	aBlock value: self selectedObject 
]

{ #category : #testing }
SpAbstractEyeElement >> shouldShowInTree [
	^ true
]

{ #category : #accessing }
SpAbstractEyeElement >> value [
	"Answers the object associated with this EyeElement."
	self subclassResponsibility
]

{ #category : #accessing }
SpAbstractEyeElement >> valueClass [
	^ self value class
]

{ #category : #accessing }
SpAbstractEyeElement >> withErrorsDo: aBlock [
	self errorPropertySelectors
		do: [ :selector | 
			self errorWhileAccessing: selector do: aBlock ].
]
