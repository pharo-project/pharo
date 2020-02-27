"
ifNotNilDo: should not be used as ifNotNil: works for blocks with arguments, too.
"
Class {
	#name : #ReRuleIfNotNilDo,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReRuleIfNotNilDo class >> uniqueIdentifierName [

	^ 'RuleIfNotNilDo'
]

{ #category : #accessing }
ReRuleIfNotNilDo >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
ReRuleIfNotNilDo >> initialize [
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
ReRuleIfNotNilDo >> name [
	^ 'Use "ifNotNil:" not "ifNotNilDo:"'
]
