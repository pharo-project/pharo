"
Replaces keys/values do: by keysDo: and valuesDo:.  This rule is based on the observation that using aDict keys do: or aDict values do: create an intermediary collection. 

Dictionary>>keys
	""Answer an Array containing the receiver's keys.""
	
	^Array new: self size streamContents: [:s| self keysDo: [:key| s nextPut: key]]
	
This array can be quite large and using the keysDo: does not create such intermediate collection.
"
Class {
	#name : #RBKeysDoRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBKeysDoRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'keysDoRule'
]

{ #category : #accessing }
RBKeysDoRule >> group [
	^ 'Optimization'
]

{ #category : #initialization }
RBKeysDoRule >> initialize [
	super initialize.
	self
		replace: '``@object keys do: ``@block' with: '``@object keysDo: ``@block';
		replace: '``@object values do: ``@block' with: '``@object valuesDo: ``@block'

]

{ #category : #accessing }
RBKeysDoRule >> name [
	^ 'keys do: -> keysDo: and valuesDo:'
]

{ #category : #accessing }
RBKeysDoRule >> rationale [
	^ 'The use of keysDo:/valuesDo: means one intermediate collection created less'
]
