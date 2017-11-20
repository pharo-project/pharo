Extension { #name : #PharoDarkTheme }

{ #category : #'*GT-Debugger' }
PharoDarkTheme >> sameClassContextStyleFor: aContext [ 

	^ {TextColor cyan}
]

{ #category : #'*GT-Debugger' }
PharoDarkTheme >> samePackageContextStyleFor: aContext [

	^ {TextColor magenta}
]

{ #category : #'*GT-Debugger' }
PharoDarkTheme >> unrelatedContextStyleFor: aContext [

	^ {TextColor color: Color white}
]
