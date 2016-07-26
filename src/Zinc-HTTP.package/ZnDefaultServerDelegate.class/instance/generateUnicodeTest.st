private
generateUnicodeTest
	"Generate an HTML page containing all Unicode characters with code points between 0 and 16r024F in a nice table"
	
	^ ZnHtmlOutputStream streamContents: [ :html |
		html page: 'Unicode Test Page' do: [ 
			html tag: #table do: [
				0 to: 16r024F by: 8 do: [ :row |
					html tag: #tr do: [
					row to: row + 7 do: [ :each |
						html tag: #td do: [ 
							html 
								nextPutAll: 'U+'; nextPutAll: (each printPaddedWith: $0 to: 4 base: 16);
								nextPut: $=; nextPut: each asCharacter ] ] ] ] ] ] ]