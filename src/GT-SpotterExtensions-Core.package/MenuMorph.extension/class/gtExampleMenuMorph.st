*GT-SpotterExtensions-Core
gtExampleMenuMorph
	<gtExample>
	| menu |
	menu := MenuMorph new.
	menu buildTitle: [ :menuTitle | menuTitle
		onlyCloseAndPinable
	].
	menu add: 'apples' selector: #apples.
	menu add: 'oranges' selector: #oranges.
	menu addLine.
	menu add: 'peaches' selector: #peaches.
	menu addLine.
	menu add: 'pears' selector: #pears.
	menu addLine.
	^ menu
	