private
addPicked: item

	pickedItemsHolder add: item.
	pickedItemsHolder valueChanged: true to: item.