instance creation
contents: aString
	" 'StringMorph contents: str' is faster than 'StringMorph new contents: str' "
	^ self contents: aString font: nil