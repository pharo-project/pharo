"
ifNotNilDo: should not be used as ifNotNil: works for blocks with arguments, too.
"
Class {
	#name : 'ReIfNotNilDoRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReIfNotNilDoRule class >> group [
	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReIfNotNilDoRule class >> ruleName [
	^ 'Use "ifNotNil:" not "ifNotNilDo:"'
]

{ #category : 'accessing' }
ReIfNotNilDoRule class >> uniqueIdentifierName [

	^ 'RuleIfNotNilDo'
]

{ #category : 'initialization' }
ReIfNotNilDoRule >> initialize [
	super initialize.
	self
		replace: '`@receiver ifNotNilDo: `@statements'
			with: '`@receiver ifNotNil: `@statements';
		replace: '`@receiver ifNotNilDo: `@statements1 ifNil: `@statements2'
			with: '`@receiver ifNotNil: `@statements1 ifNil: `@statements2';
		replace: '`@receiver ifNil: `@statements1 ifNotNilDo: `@statements2'
			with: '`@receiver ifNil: `@statements1 ifNotNil: `@statements2'
]
