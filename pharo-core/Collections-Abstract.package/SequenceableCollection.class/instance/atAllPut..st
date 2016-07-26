accessing
atAllPut: anObject 
	"Put anObject at every one of the receiver's indices."

	| size |
	(size := self size) > 26 "first method faster from 27 accesses and on"
		ifTrue: [self from: 1 to: size put: anObject]
		ifFalse: [1 to: size do: [:index | self at: index put: anObject]]