"
Use PharoDarkTheme now
"
Class {
	#name : #Pharo3DarkTheme,
	#superclass : #PharoDarkTheme,
	#category : #Deprecated70
}

{ #category : #compatibility }
Pharo3DarkTheme class >> beCurrent [
	"Make a new instance of the receiver be the current theme."

		self deprecated: '***** Use the class PharoDarkTheme now *****'.
	self current: self newDefault
]

{ #category : #testing }
Pharo3DarkTheme class >> isAbstract [
	"Answer whether the receiver is considered to be abstract."

	^true
]

{ #category : #deprecation }
Pharo3DarkTheme class >> isDeprecated [

	^true
]

{ #category : #accessing }
Pharo3DarkTheme class >> themeName [
	^ 'Pharo3 Dark (Obsolete)'
]
