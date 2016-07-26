actions examples
spawnBrowserSelectionActions
	<glmBrowser: 'Spawn browser selection actions' input: '#(1 2 3 4)'>
	"self new spawnBrowserSelectionActions openOn: #(1 2 3 4)"
	|browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		a list
			title: 'Example';
			selectionSpawn: [:listPresentation |
				|tmpBrowser|
				tmpBrowser := GLMTabulator new row: #one; initialExtent: 100@100 ;yourself.
				tmpBrowser
					transmit to: #one ; andShow: [:b |
						b text
							display: [:input | input asString]
						].
				tmpBrowser startOn: listPresentation selection
				] entitled: 'Open in new browser'
			].
	^ browser