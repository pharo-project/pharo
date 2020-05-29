"
My subclasses implement commands related to system navigation.
I give a selector from a context that must:
- be a block
- evaluate to a string or symbol
"
Class {
	#name : #SpSystemNavigationCommand,
	#superclass : #SpToolCommand,
	#category : #'Spec2-Commands-Navigation'
}

{ #category : #defaults }
SpSystemNavigationCommand class >> defaultIconName [
	^#smallSystemBrowser 
]

{ #category : #default }
SpSystemNavigationCommand class >> defaultName [
	^'Browse ', self shortName 
]

{ #category : #accessing }
SpSystemNavigationCommand class >> shortName [
	^self subclassResponsibility 
]

{ #category : #accessing }
SpSystemNavigationCommand >> selector [

	^ self target asSymbol
]
