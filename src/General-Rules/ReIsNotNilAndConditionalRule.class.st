"
Replaces isNotNil ifTrue , isNotNil ifFalse and isNotNil ifTrue:ifFalse by ifNotNil: , ifNil and ifNotNil:ifNil: to make the code more readable. Helps to avoid unnecesary temporal variables.
Works also with notNil.
"
Class {
	#name : 'ReIsNotNilAndConditionalRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReIsNotNilAndConditionalRule class >> group [

	^ 'Coding Idiom Violation'
]

{ #category : 'accessing' }
ReIsNotNilAndConditionalRule class >> rationale [

	^ 'Using specific conditional methods leads to shorter code and helps in avoiding unneeded temporary variables.<n><n>Replaces<n><t>isNotNil ifTrue: ~~> ifNotNil:<n><t>isNotNil ifFalse: ~~> ifNil:<n><t>isNotNil ifTrue:ifFalse ~~> ifNotNil:ifNil:'
		expandMacros
]

{ #category : 'accessing' }
ReIsNotNilAndConditionalRule class >> ruleName [
	^ 'Sends isNotNil / notNil and a conditional check instead of using #ifNil: #ifNotNil: or #ifNil:ifNotNil:'
]

{ #category : 'initialization' }
ReIsNotNilAndConditionalRule >> initialize [

	super initialize.
	self
		replace: '``@receiver isNotNil ifFalse: ``@nilBlock' with: '``@receiver ifNil: ``@nilBlock';
		replace: '``@receiver isNotNil ifTrue: ``@notNilBlock' with: '``@receiver ifNotNil: ``@notNilBlock';
		replace: '``@receiver isNotNil ifTrue: ``@nilBlock ifFalse: ``@notNilBlock'
			with: '``@receiver ifNotNil: ``@nilBlock ifNil: ``@notNilBlock';
		replace: '``@receiver isNotNil ifFalse: ``@notNilBlock ifTrue: ``@nilBlock'
			with: '``@receiver ifNotNil: ``@nilBlock ifNil: ``@notNilBlock';

		replace: '``@receiver notNil ifFalse: ``@nilBlock' with: '``@receiver ifNil: ``@nilBlock';
		replace: '``@receiver notNil ifTrue: ``@notNilBlock' with: '``@receiver ifNotNil: ``@notNilBlock';
		replace: '``@receiver notNil ifTrue: ``@nilBlock ifFalse: ``@notNilBlock'
			with: '``@receiver ifNotNil: ``@nilBlock ifNil: ``@notNilBlock';
		replace: '``@receiver notNil ifFalse: ``@notNilBlock ifTrue: ``@nilBlock'
			with: '``@receiver ifNotNil: ``@nilBlock ifNil: ``@notNilBlock'
]
