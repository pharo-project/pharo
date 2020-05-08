"
ifNotNilDo: should not be used as ifNotNil: works for blocks with arguments, too.
"
Class {
	#name : #ReIfNotNilDoRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReIfNotNilDoRule class >> uniqueIdentifierName [

	^ 'RuleIfNotNilDo'
]

{ #category : #accessing }
ReIfNotNilDoRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
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

{ #category : #accessing }
ReIfNotNilDoRule >> name [
	^ 'Use "ifNotNil:" not "ifNotNilDo:"'
]
