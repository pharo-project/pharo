accessing
choiceMenuItems
	"Answer the value of choiceMenus"

	^ Array streamContents: [:strm | self choiceMenus  do: [:menu | strm nextPutAll: menu menuItems]].
