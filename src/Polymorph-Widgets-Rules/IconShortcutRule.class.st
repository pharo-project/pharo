"
Your code may be more readable if you use

	#iconName asIcon
	
instead of

	Smalltalk ui icons iconNamed: #iconName
"
Class {
	#name : #IconShortcutRule,
	#superclass : #ReNodeRewriteRule,
	#category : 'Polymorph-Widgets-Rules'
}

{ #category : #accessing }
IconShortcutRule >> group [

	^ 'Style'
]

{ #category : #initialization }
IconShortcutRule >> initialize [
	super initialize.
	self
		replace: 'Smalltalk ui icons iconNamed: `@iconName'
		with: '`@iconName asIcon'
]

{ #category : #accessing }
IconShortcutRule >> name [

	^ '#iconName asIcon shortcut available'
]

{ #category : #accessing }
IconShortcutRule >> severity [

	^ #information
]
