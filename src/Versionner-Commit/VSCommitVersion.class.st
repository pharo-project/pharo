"
I ennumerate version kind (major.minor.patch)
One commit can be just one of the 3.

My children represent them clearly.

I'm used to commit projects.  

I try to represent semantic versionning. 
Please, visit: http://semver.org/ for lerning what it means.
"
Class {
	#name : #VSCommitVersion,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Versionner-Commit'
}

{ #category : #accessing }
VSCommitVersion class >> label [
	^ self subclassResponsibility
]

{ #category : #accessing }
VSCommitVersion class >> major [ 
	^ VSCommitMajorVersion uniqueInstance
]

{ #category : #accessing }
VSCommitVersion class >> minor [
	^ VSCommitMinorVersion uniqueInstance
]

{ #category : #'instance creation' }
VSCommitVersion class >> new [ 
	self error: 'Use #uniqueInstance'
]

{ #category : #accessing }
VSCommitVersion class >> parseSemanticVersionFor: versionString ifError: aBlock [ 
	"This is probably not the best way to retrieve a semantic version :P"
	| parsedVersion |
	versionString ifNil: [ ^ aBlock value ].
	
	parsedVersion := (versionString substrings: '.') asArray.
 	parsedVersion size > 3 ifTrue: [ ^ aBlock value ].
	
	parsedVersion do: [ :eachPart | 
		(eachPart allSatisfy: [ :each | each isDigit ])
			ifFalse: [ ^ aBlock value ] ].
		
	[ parsedVersion size < 3 ] 
		whileTrue: [ parsedVersion := parsedVersion copyWith: 0 ].

	^ parsedVersion collect: #asInteger
]

{ #category : #accessing }
VSCommitVersion class >> patch [
	^ VSCommitPatchVersion uniqueInstance
]

{ #category : #'instance creation' }
VSCommitVersion class >> uniqueInstance [
	self = VSCommitVersion 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #accessing }
VSCommitVersion class >> versions [
	^ self allSubclasses collect: #uniqueInstance
]

{ #category : #accessing }
VSCommitVersion >> label [
	^ self class label
]

{ #category : #private }
VSCommitVersion >> latestVersion: metacelloProject [
	| latestVersion |
	
	latestVersion := (metacelloProject versions 
		reject: [ :each |  #(baseline broken) includes: each blessing ])
		detectMax: [ :each | each ].
	^ latestVersion 
		ifNotNil: [ latestVersion versionString ]
		ifNil: [ nil ] 
]

{ #category : #accessing }
VSCommitVersion >> nextVersionOf: aProject [
	self subclassResponsibility
]

{ #category : #private }
VSCommitVersion >> semanticVersionFor: aProject [
	| projectLatestVersion |
	
	projectLatestVersion := self latestVersion: (aProject configurationClass project).
	^ self class
		parseSemanticVersionFor: projectLatestVersion
		ifError: [ 
			self crLog: 'Something went wrong trying to parse ', (projectLatestVersion asString).
			#(0 0 0) ]
]
