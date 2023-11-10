Extension { #name : 'PharoImage' }

{ #category : '*Tool-Registry' }
PharoImage >> tools [

	^ Tools ifNil: [
		Tools := PharoCommonTools new ]
]
