Extension { #name : 'SycReplaceMessageCommand' }

{ #category : '*Calypso-SystemTools-Core' }
SycReplaceMessageCommand class >> canBeExecutedInContext: aToolContext [

	self flag: #Enh.
	"The following commented piece controls the fact that the replace can be invoked from without the code pane.
	Now the asRefactoring method should have access to the class from such context and we did not find how.
	For now we removed the menu item from the list. Less working is better than more not working"
 ^ "aToolContext isMessageSelected |"
	 aToolContext isMethodSelected
]

{ #category : '*Calypso-SystemTools-Core' }
SycReplaceMessageCommand class >> methodContextMenuActivation [
	<classAnnotation>

	^ SycMethodMenuActivation byRootGroupItemOrder: 1100 for: ClyMethod asCalypsoItemContext
]
