Extension { #name : #Pragma }

{ #category : #'*Deprecated90' }
Pragma class >> withPragmasIn: aClass do: aBlock [
	self 
		deprecated: 'use #pragmasDo: on the class' 
		transformWith: 	'`@receiver withPragmasIn: `@statements1 do: `@statements2'
		   					-> '`@statements1 pragmasDo: `@statements2'.
	aClass pragmasDo: aBlock
]
