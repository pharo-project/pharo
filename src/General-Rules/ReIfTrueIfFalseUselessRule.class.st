"
it's more efficient and  more readable to return the condition that use ifTrue: ifFalse: to return a boolean. 
"
Class {
	#name : 'ReIfTrueIfFalseUselessRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReIfTrueIfFalseUselessRule >> group [

	^ 'Design Flaws'
]

{ #category : 'initialization' }
ReIfTrueIfFalseUselessRule >> initialize [

	super initialize.
	self
		replace: ' (`@condition) ifTrue: [ ^ true ] ifFalse: [ ^ false ] '
		with: '^ (`@condition)';
		replace: ' (`@condition) ifFalse: [ ^ false ] ifTrue: [ ^ true ] '
		with: '^ (`@condition)'
]

{ #category : 'accessing' }
ReIfTrueIfFalseUselessRule >> name [ 

^ 'useless ifTrue:ifFalse:'
]
