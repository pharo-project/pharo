Extension { #name : #EditorFindReplaceDialogWindow }

{ #category : #'*Polymorph-Widgets' }
EditorFindReplaceDialogWindow >> aboutText [
	^ 'This dialog can be opened with <cmd-f> in an editable text area or from the text area contextual menu. 
Previous find and replace tokens are kept so that you can directly retrieve them from the find and the replace input fields.
While you enter a find token in the find field, in the currently edited text, all matching text portions are dynamically enlighted with the find selection color (orange color by default).
The find token is shared by all text editing areas. You can also set it with the keyboard by using the <cmd-h> shortcut after having selected the portion of text you want to find. Then, you can find the next matching portion of text by using the <cmd-g> shortcut. 
In order to get rid of the current find token, just enter <cmd-h> with nothing selected or just cancel this dialog.
The colorization is manageable with the settings browser from where you can completely disallow it or change the selection color'
]

{ #category : #'*Polymorph-Widgets' }
EditorFindReplaceDialogWindow >> aboutTitle [
	^ 'Find & replace dialog'
]

{ #category : #'*Polymorph-Widgets' }
EditorFindReplaceDialogWindow >> initialize [
	"Set the initial position based on usable screen area."

	super initialize.
	self position: RealEstateAgent maximumUsableArea topLeft
]

{ #category : #'*Polymorph-Widgets' }
EditorFindReplaceDialogWindow >> removeBoxes [
	"Remove all label area boxes."

	expandBox ifNotNil: [expandBox delete. expandBox := nil].
	collapseBox ifNotNil: [collapseBox delete. collapseBox := nil]
]
