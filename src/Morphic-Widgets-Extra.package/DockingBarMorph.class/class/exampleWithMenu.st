example
exampleWithMenu
	"self exampleWithMenu"
	<exampleWidget>
	
	DockingBarMorph new 
		add: 'Foo' 
		font:  StandardFonts defaultFont 
		icon: MenuMorph pushPinImage
		help: 'No idea' subMenu: nil;
		add: 'Bar' 
		font:  StandardFonts defaultFont 
		icon: MenuMorph pushPinImage
		help: 'No idea' subMenu: nil;
	adhereToTop;
	openInWorld.