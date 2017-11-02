"
The ID for localization
"
Class {
	#name : #LocaleID,
	#superclass : #Object,
	#instVars : [
		'isoLanguage',
		'isoCountry'
	],
	#category : 'System-Localization-Locales'
}

{ #category : #accessing }
LocaleID class >> current [
	^Locale current localeID
]

{ #category : #accessing-defaults }
LocaleID class >> default [
	^ self isoLanguage: 'en'
]

{ #category : #'instance creation' }
LocaleID class >> isoLanguage: langString [
	^self isoLanguage: langString isoCountry: nil
]

{ #category : #'instance creation' }
LocaleID class >> isoLanguage: langString isoCountry: countryStringOrNil [
	^self new isoLanguage: langString isoCountry: countryStringOrNil
]

{ #category : #'instance creation' }
LocaleID class >> isoString: isoString [
	"Parse the isoString (<language>-<country>) into its components and return the matching LocaleID"
	"LocaleID isoString: 'en' "
	"LocaleID isoString: 'en-us' "

	| parts language country |
	parts := isoString findTokens: #($- ).
	language := parts first.
	parts size > 1
		ifTrue: [country := parts second].
	^self isoLanguage: language isoCountry: country
]

{ #category : #'instance creation' }
LocaleID class >> posixName: aString [ 
	^ self
		isoString: (aString copyReplaceAll: '_' with: '-')
]

{ #category : #comparing }
LocaleID >> = anotherObject [
	self class == anotherObject class
		ifFalse: [^false].
	^self isoLanguage = anotherObject isoLanguage
		and: [self isoCountry = anotherObject isoCountry]
]

{ #category : #accessing }
LocaleID >> displayCountry [
	^(ISOLanguageDefinition iso2Countries at: self isoCountry asUppercase ifAbsent: [ self isoCountry ]) 
]

{ #category : #accessing }
LocaleID >> displayLanguage [
	| language |
	language := (ISOLanguageDefinition iso2LanguageTable
				at: self isoLanguage
				ifAbsent: [^ self isoLanguage]) language.
	^ self isoCountry
		ifNil: [language]
		ifNotNil: [language , ' (' , self displayCountry , ')']
]

{ #category : #testing }
LocaleID >> hasParent [
	^self isoCountry notNil
]

{ #category : #comparing }
LocaleID >> hash [
	^self isoLanguage hash bitXor: self isoCountry hash
]

{ #category : #accessing }
LocaleID >> isoCountry [
	^isoCountry
]

{ #category : #accessing }
LocaleID >> isoLanguage [
	^isoLanguage
]

{ #category : #initialize }
LocaleID >> isoLanguage: langString isoCountry: countryStringOrNil [
	isoLanguage := langString.
	isoCountry := countryStringOrNil
]

{ #category : #accessing }
LocaleID >> isoString [
	^self asString
]

{ #category : #accessing }
LocaleID >> parent [
	^self class isoLanguage: self isoLanguage
]

{ #category : #printing }
LocaleID >> posixName [
	"(LocaleID isoString: 'es-MX') posixName"
	"(LocaleID isoString: 'es') posixName"
	"language[_territory]"
	^ self isoCountry
		ifNil: [self isoLanguage]
		ifNotNil: [self isoLanguage , '_' , self isoCountry]
]

{ #category : #printing }
LocaleID >> printOn: stream [
	"<language>-<country>"
	stream nextPutAll: self isoLanguage.
	self isoCountry
		ifNotNil: [stream nextPut: $-; nextPutAll: self isoCountry]
]

{ #category : #printing }
LocaleID >> storeOn: aStream [ 
	aStream nextPut: $(.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' isoString: '.
	aStream nextPutAll: '''' , self printString , ''''.
	aStream nextPut: $).

]
