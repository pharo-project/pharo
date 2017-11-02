Extension { #name : #String }

{ #category : #'*Polymorph-Widgets' }
String >> heightToDisplayInTree: aTree [

	^ aTree fontToUse height
]

{ #category : #'*Polymorph-Widgets-Themes' }
String class >> systemIconName [

	^ #stringIcon
]

{ #category : #'*Polymorph-Widgets' }
String >> treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph [

	aCanvas
		drawString: self
		in: drawBounds 
		font: aMorph font
		color: drawColor
]

{ #category : #'*Polymorph-Widgets' }
String >> widthToDisplayInTree: aTree [

	^ aTree fontToUse widthOfStringOrText: self contents
]
