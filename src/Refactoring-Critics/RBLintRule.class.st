"
I represent an executable check applied on an environment (groups of classes, methods... )
"
Class {
	#name : #RBLintRule,
	#superclass : #Object,
	#category : #'Refactoring-Critics'
}

{ #category : #'instance creation' }
RBLintRule class >> asRBRule [

	^ self new
]

{ #category : #testing }
RBLintRule class >> checksClass [

	^ false
]

{ #category : #testing }
RBLintRule class >> checksMethod [

	^ false
]

{ #category : #testing }
RBLintRule class >> checksPackage [

	^ false
]

{ #category : #accessing }
RBLintRule class >> identifierMinorVersionNumber [
	"This number identifies the version of the rule definition. Each time the rule is updated and its changes invalidates previous false positives identification (and as such should be reassessed by developers) the number should be increased."
	^ 1
]

{ #category : #testing }
RBLintRule class >> isUsed [
	"all my sublasses are used"
	^self name = 'RBLintRule'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : #testing }
RBLintRule class >> isVisible [
	"Answer true if the class should be visible in the GUI."

	^ false
]

{ #category : #accessing }
RBLintRule class >> uniqueIdentifierNumber [
	"This number should be unique and should change only when the rule completely change semantics"
	^ 0
]

{ #category : #'as yet unclassified' }
RBLintRule >> asRBRule [

	^ self
]

{ #category : #running }
RBLintRule >> basicCheck: anEntity [

	^ self subclassResponsibility 
]

{ #category : #accessing }
RBLintRule >> changes [
	^ #()
]

{ #category : #running }
RBLintRule >> checkClass: aClass [
	"Is a hook to specify a check that is performed at the class level"
	

]

{ #category : #running }
RBLintRule >> checkMethod: aMethod [
	"Is a hook to specify a check that is performed at the method level"
]

{ #category : #running }
RBLintRule >> checkPackage: aPackage [
	"Is a hook to specify a check that is performed at the pacakge level"
	

]

{ #category : #private }
RBLintRule >> genericPatternForSelector: aSymbol [
	^ String streamContents: [ :stream |
		aSymbol keywords keysAndValuesDo: [ :index :value |
			stream space; nextPutAll: value.
			aSymbol last = $:
				ifTrue: [ stream space; nextPutAll: '`@object'; print: index ] ] ]
]

{ #category : #accessing }
RBLintRule >> group [
	^ 'Unclassified rules'
]

{ #category : #testing }
RBLintRule >> hasConflicts [
	^ false
]

{ #category : #testing }
RBLintRule >> isArchitecturalRule [
	^ false 
]

{ #category : #testing }
RBLintRule >> isComposite [
	^ false
]

{ #category : #testing }
RBLintRule >> isEmpty [
	self subclassResponsibility
]

{ #category : #testing }
RBLintRule >> isTransformationRule [

	^ false 
]

{ #category : #accessing }
RBLintRule >> leaves [ 
	^ {self}
]

{ #category : #accessing }
RBLintRule >> longDescription [
	self
		deprecated: 'Please use #rationale  instead'
		transformWith: '`@receiver longDescription' -> '`@receiver rationale'.

	^ self rationale
]

{ #category : #accessing }
RBLintRule >> name [
	"Answer a human readable name of the rule."

	self subclassResponsibility
]

{ #category : #printing }
RBLintRule >> printOn: aStream [

	super printOn: aStream.
	self name
		ifNotNil: [ aStream
				nextPutAll: ' name: ';
				print: self name
			]
]

{ #category : #accessing }
RBLintRule >> problemCount [
	self subclassResponsibility
]

{ #category : #accessing }
RBLintRule >> rationale [
	"Answer an explanation of the rule, usually in one line. Long description can be obtained using longDescription."

	^ self class comment
]

{ #category : #running }
RBLintRule >> resetResult [
]

{ #category : #accessing }
RBLintRule >> result [
	^ self subclassResponsibility
]

{ #category : #running }
RBLintRule >> run [
	^ RBSmalllintChecker runRule: self
]

{ #category : #running }
RBLintRule >> runOnEnvironment: anEnvironment [ 
	^ RBSmalllintChecker runRule: self onEnvironment: anEnvironment
]

{ #category : #accessing }
RBLintRule >> severity [
	"Answer the severity of issues reported by this rule. This method should return one of #error, #warning, or #information."

	^ #warning
]
