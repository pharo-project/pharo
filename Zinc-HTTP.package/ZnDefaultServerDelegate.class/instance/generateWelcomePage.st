private
generateWelcomePage
	^ ZnHtmlOutputStream streamContents: [ :html |
		html html5; tag: #html do: [
			html tag: #head do: [
				html tag: #title with: 'Zinc HTTP Components'.
				html tag: #style attributes: #(type 'text/css') with: self welcomePageCss ].
		html tag: #body do: [ 
			html tag: #div id: #logo with: #Zn.
			html tag: #h1 with: 'Zinc HTTP Components'.
			html tag: #p with: self welcomePageGreeting.
			self welcomePageLinksOn: html.
			html tag: #h4 with: 'May the Source be with you!' ] ] ]