menus
addToggleItemsToHaloMenu: aMenu
	"Add standard true/false-checkbox items to the memu"

	#(#(#resistsRemovalString #toggleResistsRemoval 'whether I should be reistant to easy deletion via the pink X handle' true) #(#stickinessString #toggleStickiness 'whether I should be resistant to a drag done by mousing down on me' true) #(#lockedString #lockUnlockMorph 'when "locked", I am inert to all user interactions' true) #(#hasClipSubmorphsString #changeClipSubmorphs 'whether the parts of objects within me that are outside my bounds should be masked.' false) #(#hasDirectionHandlesString #changeDirectionHandles 'whether direction handles are shown with the halo' false) #(#hasDragAndDropEnabledString #changeDragAndDrop 'whether I am open to having objects dropped into me' false))
		do: [ :each | 
			aMenu addUpdating: each first selector: each second.
			aMenu balloonTextForLastItem: each third translated ].
	self couldHaveRoundedCorners
		ifFalse: [ ^ self ].
	aMenu addUpdating: #roundedCornersString selector: #toggleCornerRounding.
	aMenu balloonTextForLastItem: 'whether my corners should be rounded' translated