Extension { #name : 'Extension' }

{ #category : '*Monticello-Model' }
Extension >> asClassDefinition [

	| definition |
	definition := (MCExtensionDefinition named: self name)
		              extendedClass: extendedClass name;
		              packageName: self package name;
		              comment: self comment.

	"We do not export the tag if this is the root."
	self packageTag ifNotNil: [ :tag | tag isRoot ifFalse: [ definition tagName: tag name ] ].

	^ definition
]
