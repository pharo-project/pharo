menus
addExportMenuItems: aMenu hand: aHandMorph
	"Add export items to the menu"

	aMenu
		ifNotNil: [ | aSubMenu |
			aSubMenu := UIManager default newMenuIn: self for: self.
			aSubMenu add: 'BMP file' selector: #exportAsBMP.
			aSubMenu add: 'GIF file' selector: #exportAsGIF.
			aSubMenu add: 'JPEG file' selector: #exportAsJPEG.
			aSubMenu add: 'PNG file' selector: #exportAsPNG.
			aMenu
				add: 'export...'
				icon: (self iconNamed: #smallExportIcon)
				subMenu: aSubMenu.
			aMenu lastItem
				icon: (self iconNamed: #smallExportIcon) ]