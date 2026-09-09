Extension { #name : 'MorphicNativeWindow' }

{ #category : '*Morphic-Widgets-Taskbar' }
MorphicNativeWindow >> taskbarLabel [
	"Answer the label to use for a taskbar button for the receiver."
	
	^ self attributs title
]
