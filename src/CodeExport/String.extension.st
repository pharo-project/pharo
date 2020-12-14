Extension { #name : #String }

{ #category : #'*CodeExport' }
String >> writeLeadingCharRunsOn: stream [

	self isEmpty ifTrue: [^ self].

	stream nextPut: $(.
	stream print: self size.
	stream skip: -1; nextPut: $).
	stream print: 0 "was leading char".
	stream skip: -1
]
