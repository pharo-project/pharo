Extension { #name : #Pharo3DarkTheme }

{ #category : #'*GT-Debugger' }
Pharo3DarkTheme >> sameClassContextStyleFor: aContext [ 

	^ {TextColor cyan}
]

{ #category : #'*GT-Debugger' }
Pharo3DarkTheme >> samePackageContextStyleFor: aContext [

	^ {TextColor magenta}
]

{ #category : #'*GT-Debugger' }
Pharo3DarkTheme >> unrelatedContextStyleFor: aContext [

	^ {TextColor color: Color white}
]
