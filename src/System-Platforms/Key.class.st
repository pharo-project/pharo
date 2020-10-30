"
Key was deprecated in Pharo 9 in favor of KeyboardKey.
Replace your usages.

For more information, please check the comment in KeyboardKey.
"
Class {
	#name : #Key,
	#superclass : #KeyboardKey,
	#category : #'System-Platforms-Utilities'
}

{ #category : #testing }
Key class >> isDeprecated [
	^true
]
