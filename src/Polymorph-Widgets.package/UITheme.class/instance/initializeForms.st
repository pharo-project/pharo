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
		at: #windowMaximizeOver put: self newWindowMaximizeOverForm;
		at: #sbHThumbLeft put: self newScrollbarThumbLeftForm;
		at: #sbHThumbMiddle put: self newScrollbarThumbHorizontalMiddleForm;
		at: #sbHThumbRight put: self newScrollbarThumbRightForm;
		at: #sbVThumbTop put: self newScrollbarThumbTopForm;
		at: #sbVThumbMiddle put: self newScrollbarThumbVerticalMiddleForm;
		at: #sbVThumbBottom put: self newScrollbarThumbBottomForm;
		at: #buttonTopLeft put: self newButtonTopLeftForm;
		at: #buttonTopMiddle put: self newButtonTopMiddleForm;
		at: #buttonTopRight put: self newButtonTopRightForm;
		at: #buttonMiddleLeft put: self newButtonMiddleLeftForm;
		at: #buttonMiddleRight put: self newButtonMiddleRightForm;
		at: #buttonBottomLeft put: self newButtonBottomLeftForm;
		at: #buttonBottomMiddle put: self newButtonBottomMiddleForm;
		at: #buttonBottomRight put: self newButtonBottomRightForm;
		at: #buttonSquareTopLeft put: self newButtonSquareTopLeftForm;
		at: #buttonSquareTopRight put: self newButtonSquareTopRightForm;
		at: #buttonSquareBottomLeft put: self newButtonSquareBottomLeftForm;
		at: #buttonSquareBottomRight put: self newButtonSquareBottomRightForm;
		at: #buttonSelectedTopLeft put: self newButtonSelectedTopLeftForm;
		at: #buttonSelectedTopMiddle put: self newButtonSelectedTopMiddleForm;
		at: #buttonSelectedTopRight put: self newButtonSelectedTopRightForm;
		at: #buttonSelectedMiddleLeft put: self newButtonSelectedMiddleLeftForm;
		at: #buttonSelectedMiddleRight put: self newButtonSelectedMiddleRightForm;
		at: #buttonSelectedBottomLeft put: self newButtonSelectedBottomLeftForm;
		at: #buttonSelectedBottomMiddle put: self newButtonSelectedBottomMiddleForm;
		at: #buttonSelectedBottomRight put: self newButtonSelectedBottomRightForm;
		at: #buttonSquareSelectedTopLeft put: self newButtonSquareSelectedTopLeftForm;
		at: #buttonSquareSelectedTopRight put: self newButtonSquareSelectedTopRightForm;
		at: #buttonSquareSelectedBottomLeft put: self newButtonSquareSelectedBottomLeftForm;
		at: #buttonSquareSelectedBottomRight put: self newButtonSquareSelectedBottomRightForm;
		at: #radioButton put: self newRadioButtonForm;
		at: #radioButtonSelected put: self newRadioButtonSelectedForm;
		at: #checkbox put: self newCheckboxForm;
		at: #checkboxSelected put: self newCheckboxSelectedForm;
		at: #windowClosePassive put: self newWindowInactiveControlForm;
		at: #windowMinimizePassive put: self newWindowInactiveControlForm;
		at: #windowMaximizePassive put: self newWindowInactiveControlForm;
		at: #windowMenuPassive put: self newWindowMenuPassiveForm;
		at: #windowMenuOver put: self newWindowMenuOverForm
