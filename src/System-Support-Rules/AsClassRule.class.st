"
Do not use methods such as

	#asClass
	#asClassIfAbsent:
	#asClassIfPresent:
	
because they do not take into account an environment. Instead use `self class environment at: #ClassName`
"
Class {
	#name : #AsClassRule,
	#superclass : #ReNodeRewriteRule,
	#category : #System-Support-Rules
}

{ #category : #accessing }
AsClassRule >> group [

	^ 'Design Flaws'
]

{ #category : #initialization }
AsClassRule >> initialize [
	super initialize.
	self
		replace: '`@expr asClass'
		   with: 'self class environment at: `@expr';
		
		replace: '`@expr asClassIfAbsent: `@block'
		with: 'self class environment at: `@expr ifAbsent: `@block';
		
	replace: '`@expr asClassIfPresent: `@block'
		with: 'self class environment at: `@expr ifPresent: `@block'
]

{ #category : #accessing }
AsClassRule >> name [

	^ 'Do not use #asClass & similar'
]

{ #category : #accessing }
AsClassRule >> severity [

	^ #error
]
