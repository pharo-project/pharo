Extension { #name : #Object }

{ #category : #'*Deprecated90' }
Object >> notify: aString at: location [
	self 
		deprecated: 'use #notify:' 
		transformWith: '`@receiver notify: `@arg1 at: `@arg2' -> '`@receiver notify: `@arg1'.
	self notify: aString
]
