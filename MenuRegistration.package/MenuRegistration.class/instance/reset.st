initialize-release
reset
	itemList 
		ifNotNil: [
			itemList do: [:i | i reset].
			itemList := nil].
	spec := nil
