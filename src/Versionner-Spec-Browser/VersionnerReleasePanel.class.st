"
Window used to release a version.
You can specify the version number and ask for symbolic versions resolution.

self new openDialogWithSpec
"
Class {
	#name : #VersionnerReleasePanel,
	#superclass : #ComposablePresenter,
	#instVars : [
		'symbolicVersionResolutionCheckBox',
		'releaseNumber',
		'releaseNumberLabel',
		'developmentVersion'
	],
	#category : #Versionner-Spec-Browser
}

{ #category : #spec }
VersionnerReleasePanel class >> defaultSpec [
	"Define the layout of the Release Panel"

	<spec>
	^ SpecLayout composed
		newColumn: [ :c | 
					c
						newRow: [ :r1 | 
									r1
										add: #releaseNumberLabel origin: 0@0 corner: 1@1;
										add: #releaseNumber origin: 0@40 corner: 1@100]
							height: self inputTextHeight;
						newRow: [ :r2 | 
									r2 add: #symbolicVersionResolutionCheckBox ]
							height: self inputTextHeight ];
		yourself
]

{ #category : #'instance creation' }
VersionnerReleasePanel class >> openForVersion: developmentVersion [
	| releasePanel |
	
	releasePanel := self new.
	releasePanel developmentVersion: developmentVersion.
	^ releasePanel openDialogWithSpec
]

{ #category : #confirmation }
VersionnerReleasePanel >> confirmSymbolicVersionResolutionDesactivation [
	(UIManager default confirm: 'Are you sure you do not want to resolve symbolic versions? It will lead to unreproductable loads.')
		ifFalse: [ symbolicVersionResolutionCheckBox state: true ]
]

{ #category : #accessing }
VersionnerReleasePanel >> developmentVersion: aVersion [
	developmentVersion := aVersion
]

{ #category : #initialization }
VersionnerReleasePanel >> initialExtent [
	^ 500 @ 140
]

{ #category : #api }
VersionnerReleasePanel >> initialVersion: aString [
	releaseNumber text: aString.
]

{ #category : #initialization }
VersionnerReleasePanel >> initializeWidgets [
	(releaseNumberLabel := self newLabel)
		label: 'Version number:'.
	(releaseNumber := self newTextInput)
		autoAccept: true.

	(symbolicVersionResolutionCheckBox := self newCheckBox)
		label: 'Resolve symbolic dependencies? (e.g. stable -> 3.0)';
		state: true;
		whenBuiltDo: [ symbolicVersionResolutionCheckBox labelOnLeft.
						releaseNumber accept: self nextVersionString ];
		whenDeactivatedDo: [ self confirmSymbolicVersionResolutionDesactivation ]
]

{ #category : #api }
VersionnerReleasePanel >> isSymbolicVersionResolutionChecked [
	^ symbolicVersionResolutionCheckBox state
]

{ #category : #private }
VersionnerReleasePanel >> latestVersionString [
	| latestVersion |
	
	latestVersion := (developmentVersion project versions 
		reject: [ :aVersion |  #(baseline broken) includes: aVersion blessing ])
		detectMax: [ :aVersion | aVersion ].
	^ latestVersion 
		ifNotNil:  [ latestVersion versionString ]
		ifNil: [ nil ] 
]

{ #category : #private }
VersionnerReleasePanel >> nextVersionString [
	^ self latestVersionString 
		ifNotNil:  [ :versionString | | lastPointIndex |
					lastPointIndex := versionString 
						lastIndexOf: $. 
						ifAbsent: [ nil ].
					lastPointIndex 
						ifNil: [ (versionString asInteger + 1) asString ]
						ifNotNil: [ | newMinorVersion |
								newMinorVersion := (versionString allButFirst: lastPointIndex) asInteger + 1.
								(versionString  copyFrom: 1 to: lastPointIndex) , (newMinorVersion asString) ] ]
		ifNil: [ '1.0' ] 
]

{ #category : #accessing }
VersionnerReleasePanel >> releaseNumber [
	^ releaseNumber
]

{ #category : #accessing }
VersionnerReleasePanel >> releaseNumberLabel [
	^ releaseNumberLabel
]

{ #category : #accessing }
VersionnerReleasePanel >> releaseNumberText [
	^ releaseNumber text
]

{ #category : #accessing }
VersionnerReleasePanel >> symbolicVersionResolutionCheckBox [
	^ symbolicVersionResolutionCheckBox
]

{ #category : #initialization }
VersionnerReleasePanel >> title [
	^ 'Release a new version'
]
