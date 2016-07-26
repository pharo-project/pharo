accessing
userString
	"Do I have a text string to be searched on?"

	^ String streamContents: [:strm |
		1 to: self getListSize do: [:i |
			strm nextPutAll: (self getListItem: i); cr]]