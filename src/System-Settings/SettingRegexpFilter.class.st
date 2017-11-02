"
A SettingRegexpFilter is SettingFilter which makes use of a regular expression in order to keep or not a SettingHandler.

Instance Variables
	regexp:		<String>

regexp
	- a regular expression string

"
Class {
	#name : #SettingRegexpFilter,
	#superclass : #SettingFilter,
	#instVars : [
		'regexp'
	],
	#category : 'System-Settings-Filter'
}

{ #category : #filtering }
SettingRegexpFilter >> keepSettingDeclaration: aSettingDeclaration [
	^ ({aSettingDeclaration name. aSettingDeclaration label. aSettingDeclaration description}
				collect: [:e | e ifNil: ['']])
				anySatisfy: [:token | token  matchesRegexIgnoringCase: self regexp]

]

{ #category : #filtering }
SettingRegexpFilter >> keepSettingTreeNode: aSettingNode [ 
	^ (({aSettingNode methodClass name. aSettingNode selector}
			collect: [:e | e ifNil: ['']])
			anySatisfy: [:token | token substrings anySatisfy: [:sub | sub matchesRegexIgnoringCase: self regexp]])
		or: [self keepSettingDeclaration: aSettingNode settingDeclaration] 
]

{ #category : #accessing }
SettingRegexpFilter >> regexp [
	"Answer the value of regexp"

	^ regexp
]

{ #category : #accessing }
SettingRegexpFilter >> regexp: anObject [
	"Set the value of regexp"

	regexp := anObject
]
