Extension { #name : 'RGEnvironment' }

{ #category : '*Pharo30Bootstrap' }
RGEnvironment >> allClasses [

	^ self ask behaviors
]

{ #category : '*Pharo30Bootstrap' }
RGEnvironment >> includesClassNamed: aName [ 

	^ self ask includesClassNamed: aName 
]
