torevisit-tests - instance creation
testWithColumns

	| grid |
	grid := self gridClass withColumns:
 #( #('A Time to Kill' 'Blood and Smoke') ('John Grisham' 'Stephen King') ( '100' '1000') ).
	self assert: grid extent equals: 3 @ 2.
	
