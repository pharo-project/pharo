converting
printPath: aPath on: aStream
	| hasDrive |
	aPath isRoot
		ifTrue: [ ^ self ].	"effectively Windows root is empty string"
	aPath isWorkingDirectory
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath isRelative
		ifTrue: [ ^ aPath printOn: aStream delimiter: self delimiter ].
	aPath segments first first = $:
		ifTrue: [ ^ aStream nextPut: self delimiter ].	"as tagged in #pathFromString:  "
	hasDrive := aPath segments first second = $:.
	(hasDrive not )
		ifTrue: [ aStream nextPut: self delimiter ].
	aPath printOn: aStream delimiter: self delimiter.
	(hasDrive and: [ aPath segments size = 1 ])
		ifTrue: [ aStream nextPut: self delimiter ]