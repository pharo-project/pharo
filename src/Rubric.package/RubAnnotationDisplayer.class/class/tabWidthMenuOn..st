menus
tabWidthMenuOn: aBuilder
	"Specify the menu used to select the tab width"

	<contextMenu>
	<RubTabWidthMenu>
	(6 to: 200 by: 6) do: [ :t | (aBuilder item: t asString asSymbol) action: [ :model | model chosenTabWidth: t ] ] 