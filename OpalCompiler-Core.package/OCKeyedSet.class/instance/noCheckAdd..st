private
noCheckAdd: anObject
 	array at: (self findElementOrNil: (keyBlock value: anObject)) put: anObject.
 	tally := tally + 1