"
I an the root of all quality rules rules.

If you want to learn about the features of rules or how to create your own, please read:

RenrakuRuleHelp readInHelpBrowser
"
Class {
	#name : #ReAbstractRule,
	#superclass : #Object,
	#instVars : [
		'one'
	],
	#classInstVars : [
		'enabled'
	],
	#category : #'Renraku-Rules'
}

{ #category : #conversion }
ReAbstractRule class >> asRBRule [

	^ self new asRBRule
]

{ #category : #'testing-interest' }
ReAbstractRule class >> checksClass [

	^ false
]

{ #category : #'testing-interest' }
ReAbstractRule class >> checksMethod [

	^ false
]

{ #category : #'testing-interest' }
ReAbstractRule class >> checksNode [

	^ false
]

{ #category : #'testing-interest' }
ReAbstractRule class >> checksPackage [

	^ false
]

{ #category : #accessing }
ReAbstractRule class >> enabled [

	^ enabled ifNil: [ enabled := true ]
]

{ #category : #accessing }
ReAbstractRule class >> enabled: aBoolean [
	enabled := aBoolean.
	ReRuleManager reset
]

{ #category : #accessing }
ReAbstractRule class >> enabledSettingID [

	^ (self name, '_enabled') asSymbol
]

{ #category : #manifest }
ReAbstractRule class >> identifierMinorVersionNumber [
	"This number identifies the version of the rule definition. Each time the rule is updated and its changes invalidates previous false positives identification (and as such should be reassessed by developers) the number should be increased."
	^ 1
]

{ #category : #testing }
ReAbstractRule class >> isUsed [
	"all my sublasses are used"
	^self name = 'ReAbstractRule'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : #testing }
ReAbstractRule class >> isVisible [
	
	^ self methodOfInteresetSelectors anySatisfy: [ :selector |
		self perform: selector ]
]

{ #category : #properties }
ReAbstractRule class >> methodOfInteresetSelectors [

	^ #(checksMethod checksClass checksPackage checksNode)
]

{ #category : #manifest }
ReAbstractRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^ self name
]

{ #category : #manifest }
ReAbstractRule class >> uniqueIdentifierNumber [
	"This number should be unique and should change only when the rule completely change semantics"
	^ 0
]

{ #category : #helpers }
ReAbstractRule >> anchorFor: anEntity [

	^ ReSourceAnchor
			entity: anEntity
]

{ #category : #conversion }
ReAbstractRule >> asRBRule [

	^ RBRenrakuWrapperLintRule new: self
]

{ #category : #running }
ReAbstractRule >> basicCheck: anEntity [

	^ self subclassResponsibility 
]

{ #category : #running }
ReAbstractRule >> check: anEntity [

	| critiques |
	
	critiques := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critique | critiques add: critique ].
	^ critiques
]

{ #category : #running }
ReAbstractRule >> check: anEntity forCritiquesDo: aCriticBlock [

	(self basicCheck: anEntity)
		ifTrue: [ aCriticBlock cull: (self critiqueFor: anEntity) ]
]

{ #category : #running }
ReAbstractRule >> check: anEntity forCritiquesDo: aCriticBlock ifNone: alternativeBlock [
	| encounteredCritique |
	
	encounteredCritique := false.

	self check: anEntity forCritiquesDo: [ :crit |
		encounteredCritique := true.
		aCriticBlock cull: crit ].
	
	encounteredCritique ifFalse: alternativeBlock
]

{ #category : #running }
ReAbstractRule >> check: anEntity ifNoCritiques: alternativeBlock [
	| critiques |
	
	critiques := OrderedCollection new.
	self check: anEntity forCritiquesDo: [ :critique | critiques add: critique ] ifNone: alternativeBlock.
	^ critiques
]

{ #category : #helpers }
ReAbstractRule >> critiqueFor: anEntity [

	^ ReTrivialCritique
		withAnchor: (self anchorFor: anEntity)
		by: self
]

{ #category : #accessing }
ReAbstractRule >> group [
	^ 'Unclassified rules'
]

{ #category : #testing }
ReAbstractRule >> isComposite [

	^ false
]

{ #category : #accessing }
ReAbstractRule >> name [
	"Answer a human readable name of the rule."

	self subclassResponsibility
]

{ #category : #accessing }
ReAbstractRule >> rationale [
	"Answer an explanation of the rule, usually in one line. Long description can be obtained using longDescription."

	^ self class comment
]

{ #category : #compatibility }
ReAbstractRule >> resetResult [
]

{ #category : #accessing }
ReAbstractRule >> severity [
	"Answer the severity of issues reported by this rule. This method should return one of #error, #warning, or #information."

	^ #warning
]
