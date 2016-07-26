actions examples
spawnBrowserActions
	<glmBrowser: 'Spawn browser actions' input: '#(1 2 3 4)'>
	"self new spawnBrowserActions openOn: #(1 2 3 4)"
	|browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a list
			title: 'Example'.
			].
	browser spawn: [:presentation |
				|tmpBrowser|
				tmpBrowser := GLMTabulator new row: #one; initialExtent: 200@200 ;yourself.
				tmpBrowser
					transmit to: #one ; andShow: [:b |
						b text
							display: [:input | input asString]
						].
				tmpBrowser startOn: presentation entity
				] entitled: 'Open in new browser'.
	^ browser