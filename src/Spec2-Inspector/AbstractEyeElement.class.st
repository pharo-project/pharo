"
I represent an abstract inspection element. In an EyeInspector, a eye element corresponds to a wrapper around a field element.

Public API :
label: display on left list panel of the eye inspector
description: printed on the right panel of the eye inspector
host: object inspected in the eye inspector 
value: object represented by this eye element 
"
Class {
	#name : #AbstractEyeElement,
	#superclass : #Model,
	#traits : 'TEyeInspectorActions',
	#classTraits : 'TEyeInspectorActions classTrait',
	#instVars : [
		'host'
	],
	#category : #'Spec2-Inspector-Element'
}

{ #category : #'instance creation' }
AbstractEyeElement class >> host: anObject [ 
	^ self new
		host: anObject;
		yourself
]

{ #category : #comparing }
AbstractEyeElement >> = anObject [
	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ host == anObject host
]

{ #category : #accessing }
AbstractEyeElement >> accessorCode [
	"Answers a code string to access the value from the inspector"
	self subclassResponsibility
]

{ #category : #menu }
AbstractEyeElement >> browseSubMenu: aMenu [
	
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
AbstractEyeElement >> browseValue [
	^ self value browse
]

{ #category : #action }
AbstractEyeElement >> browseValueClass [
	"Open an class browser on the selectObject (class side)"
	
	^ self valueClass browse
]

{ #category : #action }
AbstractEyeElement >> browseValueClassHierarchy [
	"Create and schedule a class list browser on the receiver's hierarchy."

	^ self valueClass browseHierarchy
]

{ #category : #action }
AbstractEyeElement >> copyAccessorCode [
	"Copy the name of the current variable, so the user can paste it into the 
	window below and work with is."

	Clipboard clipboardText: self accessorCode asText
]

{ #category : #menu }
AbstractEyeElement >> customSubMenu: aMenu [
	"Subclasse may add more menu items here"
]

{ #category : #accessing }
AbstractEyeElement >> description [
	^ self value printStringLimitedTo: 2000
]

{ #category : #menu }
AbstractEyeElement >> endSubMenu: aMenu [ 
	
	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Copy Name';
				action: [ self copyAccessorCode ];
				shortcut: $c meta ] ]
]

{ #category : #accessing }
AbstractEyeElement >> errorPropertySelectors [
	"Return a collection of selectors on this eye-element that are checked against errors.
	See #withErrorsDo: and #errorSubMenu:"
	^ #(icon label longLabel description)
]

{ #category : #menu }
AbstractEyeElement >> errorSubMenu: aMenu [
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
AbstractEyeElement >> errorWhileAccessing: selector do: aBlock [
	[ self perform: selector ]
		on: Error do: [ :error |  
			aBlock cull: error cull: selector ].
]

{ #category : #action }
AbstractEyeElement >> explorePointers [
	^ Smalltalk tools pointerExplorer openStrongOn: self value
]

{ #category : #menu }
AbstractEyeElement >> exploreSubMenu: aMenu [

	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Explore Pointers';
				action: [ self explorePointers ] ] ]
]

{ #category : #action }
AbstractEyeElement >> exploreValue [
	^ self value inspect
]

{ #category : #testing }
AbstractEyeElement >> hasError [
	self withErrorsDo: [ ^ true ].
	^ false
]

{ #category : #testing }
AbstractEyeElement >> hasSelectedObject [
	^ true
]

{ #category : #comparing }
AbstractEyeElement >> hash [
	^ host hash
]

{ #category : #accessing }
AbstractEyeElement >> host [
	"Answers the object currently inspected by the outer inspector.
	See #value for the object represented by this EyeElement."
	^ host
]

{ #category : #accessing }
AbstractEyeElement >> host: anObject [
	host := anObject
]

{ #category : #accessing }
AbstractEyeElement >> hostClass [
	^ host class
]

{ #category : #accessing }
AbstractEyeElement >> icon [
	^ (self value iconOrThumbnailOfSize: 16)
		ifNil: [ self value class systemIcon ]
]

{ #category : #action }
AbstractEyeElement >> inspectValue [
	"Bring up a non-special inspector"
	^ self value inspect
]

{ #category : #menu }
AbstractEyeElement >> inspectionMenu: aMenu [
	"specific menu for the current element"
		
	self 
		exploreSubMenu: aMenu;
		browseSubMenu: aMenu;
		customSubMenu: aMenu;
		endSubMenu: aMenu;
		errorSubMenu: aMenu.
]

{ #category : #accessing }
AbstractEyeElement >> label [
	"Answers the label associated with this EyeElement"
	self subclassResponsibility
]

{ #category : #accessing }
AbstractEyeElement >> longLabel [
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
AbstractEyeElement >> mainInspectSubMenu: aMenu [ 
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
AbstractEyeElement >> printOn: aStream [
	aStream
		print: self label;
		<< '->';
		<< (self value printStringLimitedTo: 50)
]

{ #category : #action }
AbstractEyeElement >> save: aValue [
	"Subclasse may override this"
]

{ #category : #accessing }
AbstractEyeElement >> selectedObject [
	^ self value
]

{ #category : #accessing }
AbstractEyeElement >> selectedObjectDo: aBlock [
	aBlock value: self selectedObject 
]

{ #category : #testing }
AbstractEyeElement >> shouldShowInTree [
	^ true
]

{ #category : #accessing }
AbstractEyeElement >> value [
	"Answers the object associated with this EyeElement."
	self subclassResponsibility
]

{ #category : #accessing }
AbstractEyeElement >> valueClass [
	^ self value class
]

{ #category : #accessing }
AbstractEyeElement >> withErrorsDo: aBlock [
	self errorPropertySelectors
		do: [ :selector | 
			self errorWhileAccessing: selector do: aBlock ].
]
