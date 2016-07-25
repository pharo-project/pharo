private
removePicked: item

	pickedItemsHolder remove: item ifAbsent: [ ^ self ].