"
I am an eye element for self
"
Class {
	#name : #SelfEyeElement,
	#superclass : #AbstractEyeElement,
	#category : #'Spec2-Inspector-Element'
}

{ #category : #accessing }
SelfEyeElement >> accessorCode [
	^ self label
]

{ #category : #action }
SelfEyeElement >> browseClassRefs [

	self hostClass isTrait 
		ifTrue: [self systemNavigation browseAllUsersOfTrait: self hostClass]
		ifFalse: [self systemNavigation browseAllCallsOnClass: self hostClass]
]

{ #category : #action }
SelfEyeElement >> browseInstClassVariables [

	^ self systemNavigation browseClassVariables: self hostClass
]

{ #category : #action }
SelfEyeElement >> browseInstVarDefs [

	^ self systemNavigation browseInstVarDefs: self hostClass
]

{ #category : #action }
SelfEyeElement >> browseInstVarRefs [

	^ self systemNavigation browseInstVarRefs: self hostClass
]

{ #category : #action }
SelfEyeElement >> classVarRefs [

	^ self systemNavigation browseClassVarRefs: self hostClass
]

{ #category : #menu }
SelfEyeElement >> customSubMenu: aMenu [
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
SelfEyeElement >> label [
	^ 'self'
]

{ #category : #testing }
SelfEyeElement >> shouldShowInTree [
	^ false
]

{ #category : #accessing }
SelfEyeElement >> value [
	^ host
]
