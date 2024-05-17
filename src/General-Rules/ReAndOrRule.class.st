"
Both side of #| have to evaluated while for #or: if the left hand is true, the argument is not evaluated.
It's the same for #& and #and:, & have to evaluate the both side while for #and: if the left hand is false, the argument is not evaluated. 
Moreover  #and: and or: are more readable. 
"
Class {
	#name : 'ReAndOrRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'running' }
ReAndOrRule >> basicCheck: aNode [

	^ aNode isMessage and: [ #(#& |) includes: aNode selector  ]
]

{ #category : 'accessing' }
ReAndOrRule >> group [ 

	^ 'Optimization'
]

{ #category : 'accessing' }
ReAndOrRule >> name [ 

	^ 'use and: intead of & and or: instead of |'
]
