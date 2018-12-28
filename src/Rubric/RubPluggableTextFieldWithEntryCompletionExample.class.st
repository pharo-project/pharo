Class {
	#name : #RubPluggableTextFieldWithEntryCompletionExample,
	#superclass : #Object,
	#instVars : [
		'pkgName',
		'announcer'
	],
	#category : #'Rubric-Compatibility-toBeDeprecated'
}

{ #category : #examples }
RubPluggableTextFieldWithEntryCompletionExample class >> exampleOpen [
	| st |
	st := self new newScrolledText.
	self spyAnnouncer ifTrue: [st textArea announcer open].
 	st openInWorld.
	^ st

]

{ #category : #examples }
RubPluggableTextFieldWithEntryCompletionExample class >> spyAnnouncer [
	"Set it to true if you want to observe text area announcements"
	^ false
]

{ #category : #accessing }
RubPluggableTextFieldWithEntryCompletionExample >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #menu }
RubPluggableTextFieldWithEntryCompletionExample >> getMyMenu: aMenu [
	^ nil
]

{ #category : #'user interface' }
RubPluggableTextFieldWithEntryCompletionExample >> newScrolledText [
	| st |
	st := (RubPluggableTextFieldMorph new
		on: self
		text: #pkgName
		accept: #pkgName:
		readSelection: #readMySelection:
		menu: #getMyMenu:)
		withGhostText: 'Enter an existing package name';
		entryCompletion: self packagesEntryCompletion;
		withDropListButton;
		yourself.
	st width: 300.
	^ st
]

{ #category : #accessing }
RubPluggableTextFieldWithEntryCompletionExample >> packagesEntryCompletion [
	| applicants |
	applicants := RPackageOrganizer default packages
		collect: [ :package | package packageName asString ].
	^ EntryCompletion new
		dataSourceBlock: [ :currText | applicants ];
		filterBlock: [ :currApplicant :currText | 
			 currApplicant asUppercase includesSubstring: currText asString asUppercase ]
]

{ #category : #accessing }
RubPluggableTextFieldWithEntryCompletionExample >> pkgName [
	^ pkgName
]

{ #category : #accessing }
RubPluggableTextFieldWithEntryCompletionExample >> pkgName: aPackageName [
	pkgName := aPackageName.
	^ true
]

{ #category : #'user interface' }
RubPluggableTextFieldWithEntryCompletionExample >> readMySelection: aSelection [
	^ 1 to: -1
]
