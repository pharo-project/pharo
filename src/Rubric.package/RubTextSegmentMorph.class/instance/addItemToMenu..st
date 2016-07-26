ui
addItemToMenu: aMenu

|item|
	item := RubSegmentIconMenuItemMorph new
		contents: self label;
		target: self iconBlock;
		selector: #cull:;
		arguments: {self};
		icon: self icon;
		segment: self.
		
		^ aMenu addMenuItem: item