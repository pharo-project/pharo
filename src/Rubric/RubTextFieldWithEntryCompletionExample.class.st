Class {
	#name : #RubTextFieldWithEntryCompletionExample,
	#superclass : #Object,
	#instVars : [
		'pkgName',
		'announcer'
	],
	#category : #'Rubric-Editing-Widgets-Examples'
}

{ #category : #examples }
RubTextFieldWithEntryCompletionExample class >> exampleOpen [
	| st |
	st := self new newScrolledText.
 	st openInWorld.
	^ st

]

{ #category : #accessing }
RubTextFieldWithEntryCompletionExample >> announcer [
	^ announcer ifNil: [ announcer := Announcer new ]
]

{ #category : #menu }
RubTextFieldWithEntryCompletionExample >> getMyMenu: aMenu [
	^ nil
]

{ #category : #'user interface' }
RubTextFieldWithEntryCompletionExample >> newScrolledText [
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
RubTextFieldWithEntryCompletionExample >> packagesEntryCompletion [
	| applicants |
	applicants := RPackageOrganizer default packages
		collect: [ :package | package packageName asString ].
	^ EntryCompletion new
		dataSourceBlock: [ :currText | applicants ];
		filterBlock: [ :currApplicant :currText | 
			 currApplicant asUppercase includesSubstring: currText asString asUppercase ]
]

{ #category : #accessing }
RubTextFieldWithEntryCompletionExample >> pkgName [
	^ pkgName
]

{ #category : #accessing }
RubTextFieldWithEntryCompletionExample >> pkgName: aPackageName [
	pkgName := aPackageName.
	^ true
]

{ #category : #'user interface' }
RubTextFieldWithEntryCompletionExample >> readMySelection: aSelection [
	^ 1 to: -1
]
