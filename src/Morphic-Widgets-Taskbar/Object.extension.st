Extension { #name : 'Object' }

{ #category : '*Morphic-Widgets-Taskbar' }
Object class >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^nil
]

{ #category : '*Morphic-Widgets-Taskbar' }
Object >> taskbarLabel [
	"Answer the label string for the receiver in a task bar
	or nil for the default."

	^self class taskbarLabel
]
