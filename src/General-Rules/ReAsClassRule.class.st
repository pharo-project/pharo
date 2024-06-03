"
Do not use methods such as

	#asClassIfAbsent:
	#asClassIfPresent:
	
because they do not take into account an environment. Instead use `self class environment at: #ClassName`
"
Class {
	#name : 'ReAsClassRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'accessing' }
ReAsClassRule class >> group [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReAsClassRule class >> ruleName [
	^ 'Do not use #asClass & similar'
]

{ #category : 'accessing' }
ReAsClassRule class >> severity [

	^ #error
]

{ #category : 'initialization' }
ReAsClassRule >> initialize [

	super initialize.
	self
		replace: '`@expr asClassIfAbsent: `@block' with: 'self class environment at: `@expr ifAbsent: `@block';
		replace: '`@expr asClassIfPresent: `@block' with: 'self class environment at: `@expr ifPresent: `@block'
]
