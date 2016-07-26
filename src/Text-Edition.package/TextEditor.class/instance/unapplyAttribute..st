parenblinking
unapplyAttribute: aTextAttribute
	"The user selected aTextAttribute to be removed.
	If there is a selection, unapply the attribute to the selection.
	In any case do not use the attribute for the user input (emphasisHere)"

	| thisSel interval |

	self editingState emphasisHere: (self emphasisHere copyWithout: aTextAttribute).
	self selection
		ifEmpty: [ self paragraph composeAll ]				
		ifNotEmpty: [:sel | self replaceSelectionWith: (sel removeAttribute: TextAttribute).
			self paragraph composeAll.
			self recomputeSelection]