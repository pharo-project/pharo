Extension { #name : #UITheme }

{ #category : #'*Calypso-Browser' }
UITheme >> calypsoFrozenItemColor [
	"For Pharo 6 compatibilit reason it is not separate overrides in concrete theme classes.
	But this trick still allows override this color on external themes. 
	Idea to only cover default ones which are white and dark"
	
	^self backgroundColor lightness > 0.3
		ifTrue: [ Color blue muchDarker]
		ifFalse: [ Color cyan ] 
]
