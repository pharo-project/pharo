"
A SettingTokenFilter is SettingFilter which makes use of a token (substring) in order to keep or not a SettingHandler.


Instance Variables
	token:		<String>


"
Class {
	#name : #SettingTokenFilter,
	#superclass : #SettingFilter,
	#instVars : [
		'token'
	],
	#category : #'System-Settings-Filter'
}

{ #category : #filtering }
SettingTokenFilter >> keepSettingDeclaration: aSettingDeclaration [
	| selector |
	selector := (aSettingDeclaration respondsTo: #getSelector) 
		ifTrue: [aSettingDeclaration getSelector] ifFalse: [''].
	^ (({aSettingDeclaration name.  
		aSettingDeclaration label. 
		selector asUppercase. 
		aSettingDeclaration description}
				collect: [:e | (e ifNil: ['']) asUppercase])
				includesSubstringAnywhere: self token asUppercase)

]

{ #category : #filtering }
SettingTokenFilter >> keepSettingTreeNode: aSettingNode [ 
	^ (({aSettingNode methodClass name asUppercase. aSettingNode selector asUppercase}
			collect: [:e | e ifNil: ['']])
			includesSubstringAnywhere: self token asUppercase)
		or: [self keepSettingDeclaration: aSettingNode settingDeclaration]
]

{ #category : #accessing }
SettingTokenFilter >> token [
	"Answer the value of token"

	^ token
]

{ #category : #accessing }
SettingTokenFilter >> token: aString [
	"Set the value of token"

	token := aString
]
