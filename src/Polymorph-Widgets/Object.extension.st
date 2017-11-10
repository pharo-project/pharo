Extension { #name : #Object }

{ #category : #'*Polymorph-Widgets' }
Object >> iconNamed: aSymbol [
	
	^ Smalltalk ui icons iconNamed: aSymbol
]

{ #category : #'*Polymorph-Widgets' }
Object >> okToClose [
	"Sent to models when a window closing.
	Allows this check to be independent of okToChange."
	
	^true
]

{ #category : #'*Polymorph-Widgets' }
Object >> systemIcon [
	"dont use this method. It will be deprecated.
	Tools should resolve real icon object based on icon name"
	^ self iconNamed: self systemIconName
]

{ #category : #'*Polymorph-Widgets' }
Object >> systemIconName [

	^ #classIcon
]

{ #category : #'*Polymorph-Widgets' }
Object >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^self class taskbarLabel
]

{ #category : #'*Polymorph-Widgets' }
Object class >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^nil
]
