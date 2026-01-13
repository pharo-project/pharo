Class {
	#name : 'MetacelloCopyMemberSpec',
	#superclass : 'MetacelloMemberSpec',
	#instVars : [
		'sourceName'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'actions' }
MetacelloCopyMemberSpec >> applyToMap: map [

	map
		at: self sourceName
		ifPresent: [ :aSpec |
				aSpec aboutToCopy.
				map at: self name put: (aSpec copy mergeSpec: self spec copy) ]
		ifAbsent: [ self error: 'Source spec named ' , self sourceName printString , ' not found' ]
]

{ #category : 'accessing' }
MetacelloCopyMemberSpec >> sourceName [

	^sourceName
]

{ #category : 'accessing' }
MetacelloCopyMemberSpec >> sourceName: aString [

	sourceName := aString
]
