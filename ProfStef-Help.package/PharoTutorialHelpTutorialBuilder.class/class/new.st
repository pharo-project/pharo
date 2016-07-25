instance creation
new
	"We prohibid new"
	^ (self class lookupSelector: #buildHelpTopicFrom:)  == thisContext sender method  
		ifFalse:[self error: 'Please use buildHelpTopicFrom: instead']
		ifTrue: [super new]