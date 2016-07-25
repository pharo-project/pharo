private
generateDWBench
	"Generate the dynamic DW-Bench HTML page.
	Although the date/time is variable, the page size is constant."
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'DW-Bench Dynamic' do: [ 
			html tag: #table attributes: #(border 1) do: [
				1 to: 25 do: [ :row |
					html tag: #tr do: [
						1 to: 25 do: [ :col |
							html tag: #td do: [ html print: row * col ] ] ] ] ].
			html tag: #p do: [ 
				html << Date today yyyymmdd; space; << Time now print24 ] ] ]