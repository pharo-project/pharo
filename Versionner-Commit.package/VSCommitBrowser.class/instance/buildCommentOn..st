building
buildCommentOn: a
	a text
		title: 'Comment';
		onChangeOfPort: #text act: [ :presenter | self comment: presenter text ];	
		display: [ :comment | 
			"I do not like to ask for string, but comments come from two ports, then the outcome
			 is different (a unit or a string)... this is a workaround to instantiate correctly"
			comment isString  
				ifTrue: [  self comment: comment. comment ]
				ifFalse: [  self unit comment ifNil: [ '' ] ] ]