new instance creation
withRows: anArrayOfRows
	"Create a grid based on an array of rows, where the rows have the same size, the first rows in on the op and other rows are going down."
	
	"self withRows: 
			#( #('A Time to Kill' 'John Grisham' 1989) 
			#('Blood and Smoke' 'Stephen King' 2010) 
			#('Spin' 'Robert Charles Wilson' 2006))."
	
	^ self new rows: anArrayOfRows size columns: anArrayOfRows first size contents: (anArrayOfRows flatCollect: #yourself).
