*PharoDocComment
visitMethodComments: comments
	comments do: [ :comment | self addStyle: #comment from: comment start to: comment stop ].
	PharoDocCommentNode docCommentEnabled
		ifTrue: [ comments do: [ :comment | self styleDocComment: comment ] ]