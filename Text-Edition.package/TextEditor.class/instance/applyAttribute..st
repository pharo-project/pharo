private
applyAttribute: aTextAttribute
	"The user selected aTextAttribute via shortcut, menu or other means.
	If there is a selection, apply the attribute to the selection.
	In any case use the attribute for the user input (emphasisHere)"

	self editingState emphasisHere: (Text addAttribute: aTextAttribute toArray: self emphasisHere).
	self selection
		ifEmpty: [ self paragraph composeAll ]
		ifNotEmpty: [:sel | self replaceSelectionWith: (sel addAttribute: aTextAttribute).
			self paragraph composeAll.
			self recomputeSelection]