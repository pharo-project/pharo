"
ifNotEmptyDo: should not be used as ifNotEmpty: works for blocks with arguments, too.
"
Class {
	#name : #RBRuleIfNotEmptyDo,
	#superclass : #RBTransformationRule,
	#category : #Refactoring-Critics-TransformationRules
}

{ #category : #accessing }
RBRuleIfNotEmptyDo class >> uniqueIdentifierName [

	^ 'RuleIfNotEmptyDo'
]

{ #category : #accessing }
RBRuleIfNotEmptyDo >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBRuleIfNotEmptyDo >> initialize [
	super initialize.
	self rewriteRule
		replace: '`@receiver ifNotEmptyDo: `@statements'
			with: '`@receiver ifNotEmpty: `@statements';	
		replace: '`@receiver ifNotEmptyDo: `@statements1 ifEmpty: `@statements2'
			with: '`@receiver ifNotEmpty: `@statements1 ifEmpty: `@statements2';
		replace: '`@receiver ifEmpty: `@statements1 ifNotEmptyDo: `@statements2'
			with: '`@receiver ifEmpty: `@statements1 ifNotEmpty: `@statements2'
		
]

{ #category : #accessing }
RBRuleIfNotEmptyDo >> name [
	^ 'Use "ifNotEmpty:" not "ifNotEmptyDo:"'
]
