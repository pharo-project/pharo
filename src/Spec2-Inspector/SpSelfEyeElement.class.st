"
I am an eye element for self
"
Class {
	#name : #SpSelfEyeElement,
	#superclass : #SpAbstractEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SpSelfEyeElement >> accessorCode [
	^ self label
]

{ #category : #action }
SpSelfEyeElement >> browseClassRefs [

	self hostClass isTrait 
		ifTrue: [self systemNavigation browseAllUsersOfTrait: self hostClass]
		ifFalse: [self systemNavigation browseAllCallsOnClass: self hostClass]
]

{ #category : #action }
SpSelfEyeElement >> browseInstClassVariables [

	^ self systemNavigation browseClassVariables: self hostClass
]

{ #category : #action }
SpSelfEyeElement >> browseInstVarDefs [

	^ self systemNavigation browseInstVarDefs: self hostClass
]

{ #category : #action }
SpSelfEyeElement >> browseInstVarRefs [

	^ self systemNavigation browseInstVarRefs: self hostClass
]

{ #category : #action }
SpSelfEyeElement >> classVarRefs [

	^ self systemNavigation browseClassVarRefs: self hostClass
]

{ #category : #menu }
SpSelfEyeElement >> customSubMenu: aMenu [
	"Subclasse may add more menu items here"

	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem name: 'Inst var refs...';
			action: [ self browseInstVarRefs ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Inst var defs...';
			action: [ self browseInstVarDefs ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Class var refs...';
			action: [ self classVarRefs ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Class variables';
			action: [ self browseInstClassVariables ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Class refs';
			action: [ self browseClassRefs ] ] ]
]

{ #category : #accessing }
SpSelfEyeElement >> label [
	^ 'self'
]

{ #category : #testing }
SpSelfEyeElement >> shouldShowInTree [
	^ false
]

{ #category : #accessing }
SpSelfEyeElement >> value [
	^ host
]
