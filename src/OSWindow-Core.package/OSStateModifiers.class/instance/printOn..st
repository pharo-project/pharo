printing
printOn: aStream
	aStream nextPutAll: self class name; space; nextPut: $[.
	leftAlt ifTrue: [ aStream nextPutAll: 'leftAlt'; space ].
	leftCmd ifTrue: [ aStream nextPutAll: 'leftCmd'; space ].
	leftCtrl ifTrue: [ aStream nextPutAll: 'leftCtrl'; space ].
	leftShift ifTrue: [ aStream nextPutAll: 'leftShift'; space ].
	rightAlt ifTrue: [ aStream nextPutAll: 'rightAlt'; space ].
	rightCmd ifTrue: [ aStream nextPutAll: 'rightCmd'; space ].
	rightCtrl ifTrue: [ aStream nextPutAll: 'rightCtrl'; space ].
	rightShift ifTrue: [ aStream nextPutAll: 'rightShift'; space ].
	capsLock ifTrue: [ aStream nextPutAll: 'capsLock'; space ].
	numLock ifTrue: [ aStream nextPutAll: 'numLock'; space ].
	
	aStream nextPut: $]; space; print: buttons