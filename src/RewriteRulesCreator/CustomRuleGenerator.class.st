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
		'ruleName',
		'packageName'
	],
	#category : #'RewriteRulesCreator-Core'
}

{ #category : #api }
CustomRuleGenerator >> generateRule: aRuleName forPackage: aPackageName [
	| ruleAsClass fullPackageName |
	(aRuleName isValidGlobalName and: [ aPackageName isNotNil ])
		ifFalse: [ ^ false ].
	fullPackageName := 'CustomRewriteRules'.
	aPackageName ifNotEmpty: [ fullPackageName := fullPackageName , '-', aPackageName  ].
	ruleAsClass := RBCustomTransformationRule
		subclass: aRuleName
		instanceVariableNames: ''
		classVariableNames: ''
		package: fullPackageName.
	ruleAsClass
		compile: self ruleInitializationMethod
		classified: 'initialization'.
	^ true
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
	searchFor := ''' , searchFor
		,
			'''.
	replaceWith := ''' , replaceWith
		,
			'''.
	input := ''' , input
		,
			'''.
	output := ''' , output
		,
			'''.
	rewriteRule replace: searchFor with: replaceWith'
]

{ #category : #accessing }
CustomRuleGenerator >> searchFor: anObject [
	searchFor := anObject
]
