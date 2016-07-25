private
addCommentsTo: aNode
	aNode comments: aNode comments , comments.
	comments := OrderedCollection new