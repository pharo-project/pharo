"
This smell arises when a breakpoint,  logging statement,  etc is found in a method. This debugging code should not be left in production code.  Here are messages currently checked: clearHaltOnce,  doOnlyOnce: ,  halt, halt:   onCount:  object2, haltOnCount: , haltOnce, hatIf: , inspectOnCount: , inspectOnce, inspectUntilCount: , rearmOneShot, setHaltOnce, flag: , isThisEverCalled, isThisEverCalled: , logEntry, logExecution, logExit, needsWork and Transcript  messages.
"
Class {
	#name : #ReCodeCruftLeftInMethodsRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
ReCodeCruftLeftInMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'CodeCruftLeftInMethodsRule'
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> addRuleRemoving: patternString [
	"When you are completely removing statement(s), you can not just match the relevant node. You must match the whole method and then replace it minus the part to be removed"

	| findString replaceString methodTemplate |
	methodTemplate := '| `@Temps |
``@.Statements1.
{1}.
``@.Statements2'.
	findString := methodTemplate format: { patternString }.
	replaceString := methodTemplate format: { '' }.
	self replace: findString with: replaceString
]

{ #category : #helpers }
ReCodeCruftLeftInMethodsRule >> anchorFor: aNode [
	"we do not use the method of the superclass as it hightlights far too much"
    ^ ReIntervalSourceAnchor
            entity: aNode
            interval: (1 to: aNode methodNode selector size).
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> debuggingPatterns [
	| result |
	result := self debuggingSelectors collect: [ :e | self patternFor: e ].
	^ result, { 'Transcript `@message: `@arg' }
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> debuggingSelectors [
	
	^ (Object allSelectorsInProtocol: 'flagging'),
	(Object allSelectorsInProtocol: 'debugging')
]

{ #category : #accessing }
ReCodeCruftLeftInMethodsRule >> group [
	^ 'Bugs'
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> haltPatterns [
	| result |
	result := self haltSelectors collect: [ :e | self patternFor: e ].
	^ result, { 'Halt `@message: `@arg' }
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> haltSelectors [
	| objectConvenience miscellaneous |
	objectConvenience := Object allSelectorsInProtocol: #halting.
	miscellaneous := #(inspectOnce).
	^ objectConvenience, miscellaneous
]

{ #category : #initialization }
ReCodeCruftLeftInMethodsRule >> initialize [
	super initialize.
	self patterns do: [ :halt | self addRuleRemoving: halt ]
]

{ #category : #accessing }
ReCodeCruftLeftInMethodsRule >> name [
	^ 'Debugging code left in methods'
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> patternFor: selector [
	selector isUnary ifTrue: [ ^ '`@object ', selector ].
	selector isBinary ifTrue: [ ^ '`@object ', selector, ' `@arg' ].
	^ self patternForKeywordSelector: selector
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> patternForKeywordSelector: selector [
	|  index |
	^ String streamContents: [ :str |
		str nextPutAll: '`@object '.
		index := 1.
		selector keywords
			do: [ :keyword | 
				str
					nextPutAll: keyword;
					nextPutAll: ' `@arg';
					print: index ]
			separatedBy: [ 
				index := index + 1.
				str space ] ]
]

{ #category : #private }
ReCodeCruftLeftInMethodsRule >> patterns [
	^ self debuggingPatterns, self haltPatterns
]

{ #category : #accessing }
ReCodeCruftLeftInMethodsRule >> rationale [
	^ 'Breakpoints, logging statements, etc. should not be left in production code.'
]

{ #category : #accessing }
ReCodeCruftLeftInMethodsRule >> severity [
	^ #error
]
