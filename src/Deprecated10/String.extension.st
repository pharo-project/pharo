Extension { #name : #String }

{ #category : #'*Deprecated10' }
String >> beginsWithEmpty: prefix caseSensitive: aBoolean [ 
	self 
		deprecated:  'Please use #beginsWith:caseSensitive: and check for empty prefix explicitly' 
		transformWith:  '`@receiver beginsWithEmpty: `@args1 caseSensitive: `@args2' 
						-> ' `@args1 
									ifEmpty: [ true ] 
									ifNotEmpty: [ `@receiver beginsWith: `@args1 caseSensitive: `@args2 ]'.
	^prefix 
		ifEmpty: [ true ]
		ifNotEmpty: [ self beginsWith: prefix caseSensitive: aBoolean ]
]
