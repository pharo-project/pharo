menu & halo
addWorldToggleItemsToHaloMenu: aMenu
	"Add toggle items for the world to the halo menu"

	#(
	(hasDragAndDropEnabledString changeDragAndDrop 'whether I am open to having objects dropped into me')
	(roundedCornersString toggleCornerRounding 'whether the world should have rounded corners')) do:

		[:trip | aMenu addUpdating: trip first selector: trip second.
			aMenu balloonTextForLastItem: trip third]