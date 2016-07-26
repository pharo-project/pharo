initialize-release
initializeForms
	"Initialize the receiver's image forms."

	self forms: Dictionary new.
	self forms
		at: #checkboxMarker put: self newCheckboxMarkerForm;
		at: #radioButtonMarker put: self newRadioButtonMarkerForm;
		at: #treeExpanded put: self newTreeExpandedForm;
		at: #treeUnexpanded put: self newTreeUnexpandedForm;
		at: #windowClose put: self newWindowCloseForm;
		at: #windowMinimize put: self newWindowMinimizeForm;
		at: #windowMaximize put: self newWindowMaximizeForm;
		at: #windowMenu put: self newWindowMenuForm;
		at: #windowCloseOver put: self newWindowCloseOverForm;
		at: #windowMinimizeOver put: self newWindowMinimizeOverForm;
		at: #windowMaximizeOver put: self newWindowMaximizeOverForm