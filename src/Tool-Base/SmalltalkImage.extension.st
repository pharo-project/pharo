Extension { #name : #SmalltalkImage }

{ #category : #'*Tool-Base' }
SmalltalkImage >> resetTools [

	Tools := nil
]

{ #category : #'*Tool-Base' }
SmalltalkImage >> tools [

	^ Tools ifNil: [
		Tools := PharoCommonTools new ]
]
