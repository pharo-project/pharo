"
For a code more optimize and more readable, prefer to use deny: instead of assert:equals: when you want to verify that something is false.
"
Class {
	#name : 'ReDenyRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReDenyRule >> group [ 

	^ 'Optimization'
]

{ #category : 'initialization' }
ReDenyRule >> initialize [ 

	super initialize. 
	self replace: 'self assert: `@expr equals: false' with: 'self deny: `@expr'
	
]

{ #category : 'accessing' }
ReDenyRule >> name [

	^ 'use deny: instead of assert:equals:'
]
