fill-styles
paneColorFor: aWindow
	"Answer the pane color that should be used."
	
	| cc |
	(cc := aWindow valueOfProperty: #paneColor) 
		ifNotNil: [
			^self settings fadedBackgroundWindows
				ifTrue: [aWindow isActive
					ifTrue: [cc]
					ifFalse: [cc alphaMixed: 0.5 with: (Color white alpha: cc alpha)]]
				ifFalse: [cc]].
	cc := aWindow paneMorphs isEmptyOrNil ifFalse: [aWindow paneMorphs first color].
	aWindow setProperty: #paneColor toValue: (cc ifNil: [aWindow defaultBackgroundColor]).
	^aWindow paneColor