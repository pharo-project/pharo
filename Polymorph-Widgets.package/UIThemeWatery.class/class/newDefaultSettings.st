accessing
newDefaultSettings
	"Answer a new original default settings."
	"self defaultSettings: nil"
	^super newDefaultSettings
		standardColorsOnly: true;
		autoSelectionColor: false;
		buttonColor: (Color r: 232 g: 232 b: 232 range: 255);
		windowColor: (Color r: 200 g: 200 b: 200 range: 255);
		scrollbarColor: (Color r: 62 g: 142 b: 220 range: 255) ;
		selectionColor: (Color r: 0.0 g: 0.0 b: 0.8 alpha: 0.2) ;
		menuSelectionColor: (Color r: 81 g: 106 b: 230 range: 255) ;
		progressBarColor: (Color r: 232 g: 232 b: 232 range: 255);
		progressBarProgressColor: (Color r: 62 g: 142 b: 220 range: 255);
		yourself