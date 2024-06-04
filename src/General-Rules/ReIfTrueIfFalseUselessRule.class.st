"
it's more efficient and  more readable to return the condition that use ifTrue: ifFalse: to return a boolean. 
"
Class {
	#name : 'ReIfTrueIfFalseUselessRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'accessing' }
ReIfTrueIfFalseUselessRule class >> group [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReIfTrueIfFalseUselessRule class >> ruleName [
	^ 'useless ifTrue:ifFalse:'
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
