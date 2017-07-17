opening
open
	<script>
	| group welcome help zen about window |

	welcome := WelcomeHelp open.
	help := HelpBrowser open.

	zen := TextModel new
			text: ProfStef pharoZenValuesContents;
			title: 'Pharo Zen';
			aboutText: ProfStef aboutPharoZen;
			beForText;
			openWithSpec.
			
	about := TextModel new
			text: Smalltalk systemInformationString withCRs;
			title: 'About Pharo';
			beForText;
			openWithSpec.

	group := GroupWindowMorph new.
	group addWindow: welcome window.
	group addWindow: help window.
	group addWindow: SettingBrowser open.
	group addWindow: KeymapBrowser new openWithSpec window.
	group addWindow: zen window.
	group addWindow: about window.
	window := (group openInWindowLabeled: self title translated)
		extent: welcome window extent;
		yourself.
		
	group tabGroup selectedPageIndex: 1.

	^ window