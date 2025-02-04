Class {
	#name : 'ReClassInteractionDriver',
	#superclass : 'ReInteractionDriver',
	#instVars : [
		'packageNames',
		'packageName',
		'tagName'
	],
	#category : 'Refactoring-UI-Drivers',
	#package : 'Refactoring-UI',
	#tag : 'Drivers'
}

{ #category : 'accessing' }
ReClassInteractionDriver >> configureRefactoring [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReClassInteractionDriver >> packageName [

	^ packageName
]

{ #category : 'accessing' }
ReClassInteractionDriver >> packageName: anObject [

	packageName := anObject
]

{ #category : 'accessing' }
ReClassInteractionDriver >> packageNames [
	"Answer a <Collection> of packages to be displayed as potential targets of the new receiver's subclass"

	^ packageNames
		ifNil: [ packageNames := PackageOrganizer default packageNames ]
]

{ #category : 'accessing' }
ReClassInteractionDriver >> packageNames: anObject [

	packageNames := anObject
]

{ #category : 'accessing' }
ReClassInteractionDriver >> scopes: refactoringScopes [ 
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ReClassInteractionDriver >> tagName [

	^ tagName
]

{ #category : 'accessing' }
ReClassInteractionDriver >> tagName: anObject [

	tagName := anObject
]
