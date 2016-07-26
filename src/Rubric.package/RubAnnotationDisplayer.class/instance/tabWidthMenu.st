accessing
tabWidthMenu
	"Answer the menu to be presented to select a tab width"

	^ (PragmaMenuBuilder pragmaKeyword: #RubTabWidthMenu model: self) menu