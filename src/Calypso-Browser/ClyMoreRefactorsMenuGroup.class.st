"
I am a submenu for rarely used refactorings
"
Class {
	#name : 'ClyMoreRefactorsMenuGroup',
	#superclass : 'ClyMenuGroup',
	#category : 'Calypso-Browser-MenuGroups',
	#package : 'Calypso-Browser',
	#tag : 'MenuGroups'
}

{ #category : 'presenting' }
ClyMoreRefactorsMenuGroup >> hasLineAfter [
	^ true
]

{ #category : 'presenting' }
ClyMoreRefactorsMenuGroup >> hasLineBefore [
	^ true
]

{ #category : 'accessing' }
ClyMoreRefactorsMenuGroup >> name [
	^ 'Refactorings'
]

{ #category : 'accessing' }
ClyMoreRefactorsMenuGroup >> order [
	^10100
]
