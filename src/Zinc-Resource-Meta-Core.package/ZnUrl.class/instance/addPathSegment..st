accessing-path
addPathSegment: segment
	"Modify the receiver's path by adding segment at the end"
	
	segments isNil ifTrue: [ 
		segments := OrderedCollection new ].
	segments add: segment