Extension { #name : #Point }

{ #category : #'*ston-core' }
Point >> fromSton: stonReader [
	stonReader parseListDo: [ :each :index |
		index = 1 ifTrue: [ x := each ].
		index = 2 ifTrue: [ y := each ] ]
]

{ #category : #'*ston-core' }
Point >> stonOn: stonWriter [
	stonWriter writeObject: self streamShortList: [ :array |
		array add: x; add: y ]
]
