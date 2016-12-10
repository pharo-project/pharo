testing
shouldStyleNode: aNode
	^ self stylingEnabled
		and: [ aNode isMethod
				and: [ aNode comments isNotEmpty
						and: [ aNode comments anySatisfy: [ :commentNode | commentNode pharoDocCommentNodes isNotEmpty ] ] ] ]