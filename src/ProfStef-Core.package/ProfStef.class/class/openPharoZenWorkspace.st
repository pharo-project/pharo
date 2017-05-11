zen
openPharoZenWorkspace
	"Open a Workspace showing the Pharo Zen values in random order"

	"self openPharoZenWorkspace"

	TextModel new
		text: self pharoZenValuesContents;
		title: 'Pharo Zen' translated;
		aboutText: self aboutPharoZen;
		beForText;
		openWithSpec