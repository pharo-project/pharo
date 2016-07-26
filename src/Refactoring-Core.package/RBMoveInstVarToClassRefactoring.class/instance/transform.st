transforming
"protocol: transforming"
transform
	| brokenSenders |
	class addInstanceVariable: variableName asString.
	oldClass removeInstanceVariable: variableName asString.
	brokenSenders := self collectSendersOfInstVar.
	brokenSenders ifNotEmpty: [ 
		Smalltalk tools messageList
			browse: (brokenSenders collect:#method)
			title: 'Broken methods' 
			autoSelect: brokenSenders first selector] 