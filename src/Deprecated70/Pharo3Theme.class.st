"
Use PharoLightTheme now
"
Class {
	#name : #Pharo3Theme,
	#superclass : #PharoLightTheme,
	#category : #Deprecated70
}

{ #category : #compatibility }
Pharo3Theme class >> beCurrent [
	"Make a new instance of the receiver be the current theme."

	self deprecated: '***** Use the class PharoLightTheme now *****'.
	self current: self newDefault
]

{ #category : #testing }
Pharo3Theme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^true
]

{ #category : #testing }
Pharo3Theme class >> isDeprecated [

	^true
]

{ #category : #accessing }
Pharo3Theme class >> themeName [
	^ 'Pharo3 (Obsolete)'
]
