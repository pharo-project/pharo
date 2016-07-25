instance creation
on: aModel unselectedItems: unselectedItems selectedItems: selectedItems selectedItemsSetterSelector: anotherSelector title: aString unselectedItemsLabel: unselectedLabel selectedItemsLabel: selectedLabel
	| instance |
	instance := self new 
					model: aModel;
					unselectedItems: unselectedItems;
					selectedItems: selectedItems;
					selectedItemsSetterSelector: anotherSelector;
					title: aString;
					unselectedLabel: unselectedLabel;
					selectedLabel: selectedLabel.
	^instance.