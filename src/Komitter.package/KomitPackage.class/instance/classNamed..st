accesing
classNamed: aString 
	
	^ classes at: aString ifAbsentPut: [ KomitClass new trackedClass: aString  ]