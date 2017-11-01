"
I'm a simple browser to see and commit new versions of configurations. 
Configurations needs to conform semantic versioning conventions (http://semver.org) and be compatible with Verisonner. 

It works also on comit units, so the best (and only :P) way to access it is: 

(VSCommitUnit patch: ConfigurationOfVersionner) open.

"
Class {
	#name : #VSCommitBrowser,
	#superclass : #Object,
	#instVars : [
		'unit',
		'window'
	],
	#classInstVars : [
		'commentCache'
	],
	#category : 'Versionner-Commit-UI'
}

{ #category : #cleanup }
VSCommitBrowser class >> cleanUp [ 
	commentCache := nil
]

{ #category : #accessing }
VSCommitBrowser class >> commentCache [
	^ commentCache ifNil: [ commentCache := HistoryCollection new maxSize: self maxCommentCache ]
]

{ #category : #accessing }
VSCommitBrowser class >> maxCommentCache [
	^ 10
]

{ #category : #'instance creation' }
VSCommitBrowser class >> openOn: aCommitUnit [
	^ self new openOn: aCommitUnit
]

{ #category : #actions }
VSCommitBrowser >> action: action nextVersion: value [
	self 
		assert: action isProject 
		description: 'Not a project!'. 
	VSCommitVersion
		parseSemanticVersionFor: value 
		ifError: [ self error: 'Not a semantic version number!' ].		

	action nextVersion: value
]

{ #category : #actions }
VSCommitBrowser >> actionBrowse: anAction [
	anAction browseOn: self
]

{ #category : #actions }
VSCommitBrowser >> actionRemove: anAction [
	(UIManager default confirm: ('Exclude {1} from this release/commit?' format: { anAction displayString })) ifFalse: [ ^ self ].
	
	anAction removeOn: self unit.
	"Do not refresh after removing: refresh will force a 
	 recalculation of commit unit"
]

{ #category : #actions }
VSCommitBrowser >> browsePackage: aPackage [
	| package |
	package := RPackageOrganizer default packageNamed: aPackage packageName.
	Smalltalk tools browser openOnPackage: package
]

{ #category : #actions }
VSCommitBrowser >> browseProject: aProject [
	Smalltalk tools browser openOnClass: aProject configurationClass
	
]

{ #category : #building }
VSCommitBrowser >> buildBrowser [
	| browser |
	browser := GLMTabulator new
		title: [ self title ];
		row: #actions;
		row: #comment;
		row: #oldComment size: 35;
		yourself.
	browser transmit
		to: #actions;
		andShow: [ :a | 
			a table
				title: 'Dirty packages';
				display: [ self unit allActions ];
				act: [ :presenter | 
					self configure.
					presenter pane browser update ]
					icon: (Smalltalk ui icons iconNamed: #smallConfigurationIcon)
					entitled: 'Configure commit options';
				act: [ :presenter | 
					self refresh.
					presenter pane browser update ]
					icon: (Smalltalk ui icons iconNamed: #glamorousRefresh)
					entitled: 'Refresh';
				act: [ :presenter | 
					self commit.
					presenter pane browser update ]
					icon: (Smalltalk ui icons iconNamed: #smallDoItIcon)
					entitled: 'Commit';
				selectionAct: [ :presenter | self actionBrowse: presenter selection ]
					on: $b
					entitled: 'Browse';
				selectionAct: [ :presenter | 
					self actionRemove: presenter selection.
					presenter update ]
					entitled: 'Remove from this release/commit';
				icon: [ :action | action icon ];
				column: 'Name'
					evaluated: [ :action | ' ' , action displayString ];
				column: 'Version' evaluated: [ :action | action currentString ];
				column: 'Next'
					evaluated: [ :action | (action nextVersionOn: self unit) ifNil: [ '' ] ]
					modified: [ :value :action :presenter | 
						[ self action: action nextVersion: value ]
							on: Error
							do: [ :e | 
								self inform: e messageText.
								presenter pane browser update ] ]
					width: 80;
				column: 'Repository' evaluated: [ :action | action repository ] ].
	browser transmit
		to: #comment;
		andShow: [ :a | self buildCommentOn: a ].
	browser transmit
		to: #oldComment;
		andShow: [ :a | a dropDownList display: [ self oldComments ] ].
	browser transmit
		from: #oldComment;
		to: #comment;
		andShow: [ :a | self buildCommentOn: a ].
	^ browser
]

{ #category : #building }
VSCommitBrowser >> buildCommentOn: a [
	a text
		title: 'Comment';
		onChangeOfPort: #text act: [ :presenter | self comment: presenter text ];	
		display: [ :comment | 
			"I do not like to ask for string, but comments come from two ports, then the outcome
			 is different (a unit or a string)... this is a workaround to instantiate correctly"
			comment isString  
				ifTrue: [  self comment: comment. comment ]
				ifFalse: [  self unit comment ifNil: [ '' ] ] ]
]

{ #category : #actions }
VSCommitBrowser >> comment: comment [
	| commentString |
	commentString := comment asString trimBoth.
	commentString ifEmpty: [ ^ self ].
	self unit comment: commentString
]

{ #category : #actions }
VSCommitBrowser >> commit [
	[  
		UIManager default 
			informUser: 'Executing ', self title 
			during: [ self unit execute ] ]
	on: VSCommitProjectValidationError 
	do: [ :e | ^ self informValidationError: e ].
	self unit projectVersion 
		ifNotNil: [ :version | version open ].
	self unit comment 
		ifNotNil: [ :comment | 
			self class commentCache add: comment.
			self unit comment: nil ].
	self refresh
]

{ #category : #accessing }
VSCommitBrowser >> commitVersionKind [
	^ self unit commitVersion label asUppercase
]

{ #category : #accessing }
VSCommitBrowser >> configurationName [
	^ self unit configurationClass name
]

{ #category : #actions }
VSCommitBrowser >> configure [
	| strategy options result |

	strategy := self unit commitStrategy copy. 
	options := strategy options.
	result := (TickDialogWindow 
		itemsList: options
		itemsHeaderName: '' 
		wrapBlockOrSelector: [ :item | item first ]
		title: 'Options') 
		selectItems: (options select: [ :each | strategy perform: each second ] );
		chooseFromOwner: window.
	result ifNil: [ ^ self ].
	(options difference: result) do: [ :each | strategy perform: each second asMutator with: false ]. 
	result do: [ :each | strategy perform: each second asMutator with: true ].
	
	self unit commitStrategy: strategy
]

{ #category : #private }
VSCommitBrowser >> informValidationError: anError [
	UIManager default inform: (String streamContents: [ :stream |
		stream 
			<< anError project name << ' has errors:'; cr. 
		anError issues 
			do: [ :each | stream << each ]
			separatedBy: [ stream cr ] ])
]

{ #category : #accessing }
VSCommitBrowser >> oldComments [
	^ self class commentCache elements
]

{ #category : #showing }
VSCommitBrowser >> openOn: anObject [
	unit := anObject.
	window := self buildBrowser openOn: anObject
]

{ #category : #actions }
VSCommitBrowser >> refresh [
	self unit refresh
]

{ #category : #accessing }
VSCommitBrowser >> title [
	^ '{1} commit for {2}' format: { self commitVersionKind. self configurationName }
]

{ #category : #accessing }
VSCommitBrowser >> unit [
	^ unit
]
