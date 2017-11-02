"
I am a subclass of StandardWindow with my own icon

Maybe I'm useless, but right now I'm not on the top of the to do list
"
Class {
	#name : #NautilusWindow,
	#superclass : #StandardWindow,
	#category : 'NautilusCommon'
}

{ #category : #about }
NautilusWindow >> aboutText [

	^'Nautilus is a new browser for Pharo Smalltalk based on RPackage and Announcements with fancy goodies:
	- groups
	- multi-selections
	- environments
	- iconic buttons
	- hierarchy
	- pragma based menus
	- KeyMapping shortcuts
	- Ring compliant
	- refactorings support
	- stateless shortcuts
	- multiple source code panel
	- direct feedback (method too long, class uncommented ...)

Made by Benjamin Van Ryseghem and friends'
]

{ #category : #about }
NautilusWindow >> aboutTitle [

	^'About Nautilus'
]

{ #category : #open/close }
NautilusWindow >> delete [
	
	| mod |
	mod := self model.
	super delete.
	mod 
		ifNotNil: [ mod announce: (WindowClosed new window: self) ]
]

{ #category : #'event handling' }
NautilusWindow >> hasFocus [

	^ ActiveHand keyboardFocus == self
]

{ #category : #open/close }
NautilusWindow >> initialExtent [
	^ 850@620
]
