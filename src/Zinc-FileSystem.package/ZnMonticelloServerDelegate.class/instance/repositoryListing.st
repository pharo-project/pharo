private
repositoryListing
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Monticello Repository' do: [
			html tag: #ul do: [ 
				self self mczEntries do: [ :each |
					html tag: #li do: [ 
						html 
							tag: #a 
							attributes: { #href. each } 
							with: each ] ] ] ] ]