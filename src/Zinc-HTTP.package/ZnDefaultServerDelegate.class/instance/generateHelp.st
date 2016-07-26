private
generateHelp
	"Generate an HTML page with links to all pages I support"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: (self class name, ' Help') do: [  	
			html tag: #h3 with: 'Available Pages'.
			html tag: #ul do: [
				prefixMap keys sorted do: [ :each | 
					html tag: #li do: [ 
						html tag: #a attributes: { #href. each } with: each ] ] ] ] ]