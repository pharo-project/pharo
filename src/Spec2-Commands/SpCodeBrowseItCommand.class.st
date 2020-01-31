Class {
	#name : #SpCodeBrowseItCommand,
	#superclass : #SpCodeBrowsingCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #default }
SpCodeBrowseItCommand class >> defaultIconName [
	^#smallSystemBrowser
]

{ #category : #default }
SpCodeBrowseItCommand class >> defaultName [

	^ 'Browse it'
]

{ #category : #default }
SpCodeBrowseItCommand class >> defaultShortcutKey [

	^ $b command mac 
		| $b control win
		| $b control unix
]

{ #category : #private }
SpCodeBrowseItCommand >> browseClass: aClass [
	aClass browse
]

{ #category : #execution }
SpCodeBrowseItCommand >> execute [
	"Treat the current text selection as an expression; evaluate it."
	| classToBrowse |
	
	classToBrowse := self findClassOn: self selectionOrLine trimmed.
	classToBrowse ifNil: [ ^ self ].

	self browseClass: classToBrowse
]

{ #category : #private }
SpCodeBrowseItCommand >> findClassOn: aString [
	| ast |
	
	ast := RBParser parseExpression: aString onError: [ ^ nil ].
	ast nodesDo: [ :node |
		(node isVariable and: [ node name first isUppercase ]) 
			ifTrue: [ 
				(self class environment classNamed: node name) 
					ifNotNil: [ :aClass | ^ aClass ] ] ].
		
	^ nil
]
