instance creation
withColumns: anArrayOfColumns
	"self withColumns: #( #('A Time to Kill' 'Blood and Smoke') ('John Grisham' 'Stephen King') ( '100' '1000') )"
	
	| numberOfRows numberOfColumns |
	numberOfColumns := anArrayOfColumns size.
	numberOfRows := anArrayOfColumns first size.
	^ self new rows: numberOfRows columns: numberOfColumns contents: (anArrayOfColumns flatCollect: #yourself).
