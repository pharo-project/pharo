private
formatClass: aClass
	aClass selectors
		do: [ :each | self formatClass: aClass selector: each ]