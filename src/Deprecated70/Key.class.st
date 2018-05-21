"
This class is deprecated. Use the class KeyboardKey instead of me.

I represent a keyboard Key. I am mapped from the platform specific keycodes into a common keycode base, by using my class side methods.
"
Class {
	#name : #Key,
	#superclass : #KeyboardKey,
	#category : #Deprecated70
}

{ #category : #deprecation }
Key class >> isDeprecated [
	^true
]
