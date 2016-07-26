accessing
searchText: aString
	"search a text and answers a list of elements indexes matching condition. 
	 E.g,
	 	#() 		--> No match 
		#(1) 		--> match first element
		#(1 42) 	--> match elements 1 and 42
			
	Rows not in range will cause the end of the world as we know it :)"
	^ #()