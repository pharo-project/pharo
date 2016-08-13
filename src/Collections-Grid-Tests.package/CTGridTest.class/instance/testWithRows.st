torevisit-tests - instance creation
testWithRows

	| grid |
	grid := self gridClass withRows:
 #( #('A Time to Kill' 'John Grisham' '100') #('Blood and Smoke' 'Stephen King' '1000')).
	self assert: grid extent equals: 3 @ 2	
