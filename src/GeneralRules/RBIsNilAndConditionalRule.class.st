"
Replaces isNil ifTrue , isNil ifFalse and isNil ifTrue:ifFalse by ifNil: , ifNotNil and ifNil:ifNotNil: to make the code more readable. Helps to avoid unnecesary temporal variables.
"
Class {
	#name : #RBIsNilAndConditionalRule,
	#superclass : #ReNodeRewriteRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #accessing }
RBIsNilAndConditionalRule >> group [

	^ 'Coding Idiom Violation'
]

{ #category : #initialization }
RBIsNilAndConditionalRule >> initialize [

	super initialize.
	self
		replace: '``@receiver isNil ifFalse: ``@notNilBlock' with: '``@receiver ifNotNil: ``@notNilBlock';
		replace: '``@receiver isNil ifTrue: ``@nilBlock' with: '``@receiver ifNil: ``@nilBlock';
		replace: '``@receiver isNil ifTrue: ``@nilBlock ifFalse: ``@notNilBlock'
			with: '``@receiver ifNil: ``@nilBlock ifNotNil: ``@notNilBlock';
		replace: '``@receiver isNil ifFalse: ``@notNilBlock ifTrue: ``@nilBlock'
			with: '``@receiver ifNil: ``@nilBlock ifNotNil: ``@notNilBlock'
]

{ #category : #accessing }
RBIsNilAndConditionalRule >> name [

	^ 'Sends isNil and a conditional check instead of using #ifNil: #ifNotNil: or #ifNil:ifNotNil:'
]

{ #category : #accessing }
RBIsNilAndConditionalRule >> rationale [

	^ 'Using specific conditional methods leads to shorter code and helps in avoiding unneeded temporary variables.<n><n>Replaces<n><t>isNil ifTrue: ~~> ifNil:<n><t>isNil ifFalse: ~~> ifNotNil:<n><t>isNil ifTrue:ifFalse ~~> ifNil:ifNotNil:'
		expandMacros
]
