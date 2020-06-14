"
I am in charge of saving the RewriteRules. I receive the custom rule as plain text and save it in form of a class. 
"
Class {
	#name : #CustomRuleGenerator,
	#superclass : #Object,
	#instVars : [
		'input',
		'output',
		'searchFor',
		'replaceWith',
		'protocolName',
		'ruleName'
	],
	#category : #'RewriteToolCustomRules-Core'
}

{ #category : #testing }
CustomRuleGenerator >> areRuleAndProtocolNamesValid [
	^ protocolName isNotNil
		and: [ protocolName isValidGlobalName
				and: [ ruleName isNotNil and: [ ruleName isValidGlobalName ] ] ]
]

{ #category : #defaults }
CustomRuleGenerator >> classNotCreatedAlert [
	UIManager default alert: 'The name you enter is invalid. Please try again.' title: 'Alert'
]

{ #category : #api }
CustomRuleGenerator >> generateCustomRewriteRule [
	| ruleAsClass |
	ruleName := self inputClassName.
	protocolName := self inputPackageProtocol.
	self areRuleAndProtocolNamesValid
		ifFalse: [ self classNotCreatedAlert.
			^ self ].
	ruleAsClass := RBCustomTransformationRule
		subclass: ruleName
		instanceVariableNames: ''
		classVariableNames: ''
		package: 'RewriteToolCustomRules-' , protocolName.
	ruleAsClass
		compile: self ruleInitializationMethod
		classified: 'initialization'
]

{ #category : #initialization }
CustomRuleGenerator >> initialize [
	super initialize.
	input := ''.
	output := ''.
]

{ #category : #accessing }
CustomRuleGenerator >> input: anObject [
	input := anObject
]

{ #category : #defaults }
CustomRuleGenerator >> inputClassName [
	^ UIManager default
		request: 'Name of the rewrite rule class: '
		initialAnswer: ''
]

{ #category : #defaults }
CustomRuleGenerator >> inputPackageProtocol [
	^ UIManager default
		request: 'Name of the protocol of the rewrite rule package: '
		initialAnswer: 'Custom'
]

{ #category : #accessing }
CustomRuleGenerator >> output: anObject [
	output := anObject
]

{ #category : #accessing }
CustomRuleGenerator >> replaceWith: anObject [
	replaceWith := anObject
]

{ #category : #accessing }
CustomRuleGenerator >> ruleInitializationMethod [
	^ 'initialize
	super initialize.
	self searchFor: ''' , searchFor
		,
			'''.
	self replaceWith: ''' , replaceWith
		,
			'''.
	self input: ''' , input
		,
			'''.
	self output: ''' , output
		,
			'''.
	rewriteRule replace: searchFor with: replaceWith'
]

{ #category : #accessing }
CustomRuleGenerator >> searchFor: anObject [
	searchFor := anObject
]
