Extension { #name : 'SycDeprecateClassCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycDeprecateClassCommand class >> browserContextMenuActivation [
	"disabled for now, see https://github.com/pharo-project/pharo/issues/9551"
	<classAnnotation>

	^CmdContextMenuActivation byRootGroupItemOrder: 1.3 for: ClyClass asCalypsoItemContext
]

{ #category : '*Calypso-SystemTools-Core' }
SycDeprecateClassCommand class >> sourceCodeMenuActivation [
	"disabled for now, see https://github.com/pharo-project/pharo/issues/9551"
	<classAnnotation>

	^SycSourceCodeMenuActivation byRootGroupItemOrder: 1.3 for: ClySourceCodeContext
]
