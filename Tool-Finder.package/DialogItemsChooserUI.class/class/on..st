instance creation
on: aDialogItemsChooser
	^self basicNew 
		unselectedItems: aDialogItemsChooser unselectedItems;
		selectedItems: aDialogItemsChooser selectedItems;
		dialogItemsChooser: aDialogItemsChooser;
		initialize.