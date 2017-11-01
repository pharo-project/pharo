Extension { #name : #Object }

{ #category : #'*System-Support' }
Object class >> registerToolsOn: aToolRegistry [

	" Override to register any tools for Smalltalk tools registry. " 
]

{ #category : #'*System-Support' }
Object >> systemNavigation [

	^ SystemNavigation new
]
