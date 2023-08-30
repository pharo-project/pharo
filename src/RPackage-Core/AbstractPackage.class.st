Class {
	#name : #AbstractPackage,
	#superclass : #Object,
	#instVars : [
		'organizer'
	],
	#category : #'RPackage-Core-Base'
}

{ #category : #'class tags' }
AbstractPackage >> classTags [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> classes [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> definedClasses [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> extendedClasses [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> extensionSelectors [

	^ self subclassResponsibility
]

{ #category : #testing }
AbstractPackage >> isEmpty [

	^ self classes isEmpty and: [ self extensionSelectors isEmpty ]
]

{ #category : #testing }
AbstractPackage >> isTestPackage [
	"1. Test package ends with suffix -Tests. Suffix is case sensitive.
	 2. Or test package contains '-Tests-' in middle.
	Some examples: "
	"(RPackage named: 'MockPackage-Tests') isTestPackage >>> true"
	"(RPackage named: 'MockPackage-tests') isTestPackage >>> false"
	"(RPackage named: 'MockPackage') isTestPackage >>> false"
	"(RPackage named: 'MockPackage-Tests-Package') isTestPackage >>> true"

	^ (self name endsWith: '-Tests') or: [self name includesSubstring: '-Tests-']
]

{ #category : #accessing }
AbstractPackage >> name [

	^ self subclassResponsibility
]

{ #category : #private }
AbstractPackage >> organizer [

	self flag: #package.
	^ organizer ifNil: [ self class organizer ]
]

{ #category : #accessing }
AbstractPackage >> organizer: anObject [

	organizer := anObject
]

{ #category : #accessing }
AbstractPackage >> packageName [

	self flag: #package. "Deprecate and call #name instead."
	^ self name
]

{ #category : #removing }
AbstractPackage >> removeClass: aClass [

	self subclassResponsibility
]

{ #category : #'class tags' }
AbstractPackage >> removeEmptyTags [

	(self classTags select: [ :tag | tag isEmpty ]) do: [ :emptyTag | self removeTag: emptyTag ]
]

{ #category : #removing }
AbstractPackage >> removeMethod: aCompiledMethod [

	self subclassResponsibility
]

{ #category : #removing }
AbstractPackage >> removeTag: aTag [

	self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> rootTag [

	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractPackage >> rootTagName [

	self flag: #package. "In the future we should not use the name of the package because it causes bugs."
	^ self name
]

{ #category : #'class tags' }
AbstractPackage >> tagOf: aClass [

	^ self subclassResponsibility
]
