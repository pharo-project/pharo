private
welcomePageLinksOn: html
	html tag: #ul do: [ 
		html tag: #li do: [ 
			html 
				str: 'Project homepage'; space;  
				tag: #a attributes: #(href 'http://zn.stfx.eu') with: 'http://zn.stfx.eu' ].
		html tag: #li do: [ 
			html 
				str: 'Locally available pages'; space;  
				tag: #a attributes: #(href '/help') with: '/help' ] ]