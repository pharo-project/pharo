zen
openPharoZenWorkspace
	"Open a Workspace showing the Pharo Zen values in random order"

	"self openPharoZenWorkspace"

	TextModel new
		text: self pharoZenValuesContents;
		title: 'Pharo Zen';
		aboutText: self aboutPharoZen;
		beForText;
		openWithSpec