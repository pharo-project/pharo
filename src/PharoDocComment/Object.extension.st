Extension { #name : 'Object' }

{ #category : '*PharoDocComment' }
Object >> >>> anObject [

	"Return a pair. It is handy to support the tweaking of pharo doc expression. A pharo doc expression is a comment as the following one:"

	"4 + 3 >>> 7"

	"Pay attention when you write an executable comment for keyword-based method: surround with parentheses your expression because, otherwise the executable comment won't work as the message is binary."

	"(1 max: 1000) >>> 1000"

	"An executable comment must be in its own comment block (enclosed in double quotes) and can be multi-line for better readability.
	Code editor hint: you can double click on the inner side of a double quote to select the whole comment block, then Cmd+I to evaluate and inspect the whole executable comment."

	"Note: to be syntactically valid as an executable comment, there should be exactly one single triple > message send, and it should be the last statement."

	"| rectangles |
	rectangles := OrderedCollection new
		add: (Rectangle left: 5 right: 10 top: 0 bottom: 15);
		add: (Rectangle left: 0 right: 15 top: 5 bottom: 10);
		yourself.
	(Rectangle merging: rectangles)
		>>> (Rectangle left: 0 right: 15 top: 0 bottom: 15)"

	^ self -> anObject
]
