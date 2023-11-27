Extension { #name : 'RGEnvironment' }

{ #category : '*PharoBootstrap' }
RGEnvironment >> allClasses [

	^ self ask behaviors
]

{ #category : '*PharoBootstrap' }
RGEnvironment >> includesClassNamed: aName [ 

	^ self ask includesClassNamed: aName 
]
