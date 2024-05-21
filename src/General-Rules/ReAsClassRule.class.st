"
Do not use methods such as

	#asClassIfAbsent:
	#asClassIfPresent:
	
because they do not take into account an environment. Instead use `self class environment at: #ClassName`
"
Class {
	#name : 'ReAsClassRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReAsClassRule >> group [

	^ 'Design Flaws'
]

{ #category : 'initialization' }
ReAsClassRule >> initialize [

	super initialize.
	self
		replace: '`@expr asClassIfAbsent: `@block' with: 'self class environment at: `@expr ifAbsent: `@block';
		replace: '`@expr asClassIfPresent: `@block' with: 'self class environment at: `@expr ifPresent: `@block'
]

{ #category : 'accessing' }
ReAsClassRule >> name [

	^ 'Do not use #asClass & similar'
]

{ #category : 'accessing' }
ReAsClassRule >> severity [

	^ #error
]
