"
I represent simple checkbox which toggle method visibility of the full browser.

I am created on visibility level which gives me new class scope which I pass to the browser.

Internal Representation and Key Implementation Points.

    Instance Variables
	browser:		<ClyBrowser>
	visibilityLevel:		<ClyMethodVisibilityLevel>
"
Class {
	#name : #ClyMethodVisibilityMorph,
	#superclass : #Morph,
	#instVars : [
		'browser',
		'visibilityLevel'
	],
	#category : #'Calypso-SystemTools-FullBrowser-UI'
}

{ #category : #'instance creation' }
ClyMethodVisibilityMorph class >> for: aBrowser level: aMethodVisibilityLevel [
	^self new 
		browser: aBrowser;
		visibilityLevel: aMethodVisibilityLevel 
]

{ #category : #accessing }
ClyMethodVisibilityMorph >> browser [
	^ browser
]

{ #category : #accessing }
ClyMethodVisibilityMorph >> browser: anObject [
	browser := anObject
]

{ #category : #building }
ClyMethodVisibilityMorph >> build [

	| checkBox |
	checkBox := CheckboxMorph 
		on: self selected: #isMethodsVisible changeSelected: #showInheritedMethods:.
	checkBox
		borderWidth: 0;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.
	checkBox labelMorph delete.	

	self addMorph: checkBox.
	
]

{ #category : #initialization }
ClyMethodVisibilityMorph >> initialize [
	super initialize.
	color := Color transparent.
	self changeTableLayout.
	self vResizing: #shrinkWrap.
	self hResizing: #shrinkWrap.
	self minWidth: 0.
	self minHeight: 0.

]

{ #category : #testing }
ClyMethodVisibilityMorph >> isMethodsVisible [
	
	^visibilityLevel isActive
]

{ #category : #controlling }
ClyMethodVisibilityMorph >> showInheritedMethods: aBoolean [

	| newScope |
	self isMethodsVisible == aBoolean ifTrue: [ ^self ].
	
	newScope := visibilityLevel toggleScope.
	newScope isBasedOnEmptyBasis ifFalse: [ 
		browser restrictMethodVisibilityBy: newScope]
]

{ #category : #accessing }
ClyMethodVisibilityMorph >> visibilityLevel [
	^ visibilityLevel
]

{ #category : #accessing }
ClyMethodVisibilityMorph >> visibilityLevel: anObject [
	visibilityLevel := anObject
]
