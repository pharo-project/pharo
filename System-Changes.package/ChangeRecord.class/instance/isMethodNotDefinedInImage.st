testing
isMethodNotDefinedInImage
	"answer whether the method represented by the receiver is not present in the image.
	pay attention is it not just isMethodDefinedInImage not"
	
	| aClass |
	^ self type = #method and:
			[(aClass := self methodClass) isNil or:
				[(aClass includesSelector: self methodSelector) not]]