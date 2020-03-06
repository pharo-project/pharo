"
ifNotEmptyDo: should not be used as ifNotEmpty: works for blocks with arguments, too.
"
Class {
	#name : #ReRuleIfNotEmptyDoRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReRuleIfNotEmptyDoRule class >> uniqueIdentifierName [

	^ 'RuleIfNotEmptyDo'
]

{ #category : #accessing }
ReRuleIfNotEmptyDoRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReRuleIfNotEmptyDoRule >> initialize [
	super initialize.
	self
		replace: '`@receiver ifNotEmptyDo: `@statements'
			with: '`@receiver ifNotEmpty: `@statements';	
		replace: '`@receiver ifNotEmptyDo: `@statements1 ifEmpty: `@statements2'
			with: '`@receiver ifNotEmpty: `@statements1 ifEmpty: `@statements2';
		replace: '`@receiver ifEmpty: `@statements1 ifNotEmptyDo: `@statements2'
			with: '`@receiver ifEmpty: `@statements1 ifNotEmpty: `@statements2'
		
]

{ #category : #accessing }
ReRuleIfNotEmptyDoRule >> name [
	^ 'Use "ifNotEmpty:" not "ifNotEmptyDo:"'
]
