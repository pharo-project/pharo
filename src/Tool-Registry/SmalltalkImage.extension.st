Extension { #name : #SmalltalkImage }

{ #category : #'*Tool-Registry' }
SmalltalkImage >> tools [

	^ Tools ifNil: [
		Tools := PharoCommonTools new ]
]
