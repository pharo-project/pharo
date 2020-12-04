Extension { #name : #Object }

{ #category : #'*PharoDocComment' }
Object >> >>> anObject [
	"Return a pair. It is handy to support the tweaking of pharo doc expression. A pharo doc expression is a comment as the following one:"

	"4 + 3 >>> 7"
	
	"Pay attention when you write an executable comment for keyword-based method : surround with parentheses your expression because, otherwise the executable comment won't work as the message is binary."
	
	"(1 max: 1000) >>> 1000"

	^ self -> anObject 
]
