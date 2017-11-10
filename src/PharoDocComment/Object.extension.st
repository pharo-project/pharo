Extension { #name : #Object }

{ #category : #'*PharoDocComment' }
Object >> >>> anObject [
	"Return a pair. It is handy to support the tweaking of pharo doc expression. A pharo doc expression is a comment as the following one:"

	"4 + 3 >>> 7"
	"4 + 4 >>> HKJ"
	
	"so far the previous expression is not considered as faulty.
	The following is a faulty expression"
	"hjkjh >>> 6"
	^ self -> anObject 
]
