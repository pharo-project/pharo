private
importIcons: icons fromFolder: aString inClass: aClass category: aCategory
	icons
		do: [:each |
			| method form |
			form := PNGReadWriter formFromFileNamed: aString, '/', each , '.png'.
			method := each , Character cr asString ,
				(aClass methodStart: each),		
				form storeString,
				aClass methodEnd.
			aClass class compile: method classified: aCategory ].
	aClass initialize