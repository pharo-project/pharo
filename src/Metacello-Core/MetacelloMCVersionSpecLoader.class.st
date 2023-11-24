Class {
	#name : 'MetacelloMCVersionSpecLoader',
	#superclass : 'Object',
	#instVars : [
		'versionSpec',
		'required',
		'packages',
		'target'
	],
	#category : 'Metacello-Core-Loaders',
	#package : 'Metacello-Core',
	#tag : 'Loaders'
}

{ #category : 'instance creation' }
MetacelloMCVersionSpecLoader class >> on: aVersionSpec [

	^(self new) 
		versionSpec: aVersionSpec;
		yourself
]

{ #category : 'loading' }
MetacelloMCVersionSpecLoader >> executeWithEngine: anEngine forTarget: aTarget [

	"Double dispatch on the target to decide if it is a load, a fetch or a record"

	packages := Dictionary new.
	self versionSpec resolveToLoadableSpecs: required map: packages.
	packages values do: [ :pkg |
		pkg ensureLoadedForDevelopmentUsing: aTarget withEngine: anEngine.
		(MetacelloIgnorePackageLoaded signal: pkg) ifFalse: [
			anEngine ignoreImage ifFalse: [
				(pkg
					 compareCurrentVersion: aTarget operator
					 targetVersionStatus: #( #allLoadedToSpec )
					 using: aTarget
					 withEngine: anEngine) ifTrue: [ packages removeKey: pkg name ] ] ] ].
	packages notEmpty ifTrue: [
		aTarget preLoad: self versionSpec withEngine: anEngine.
		aTarget load: self versionSpec withEngine: anEngine.
		aTarget postLoad: self versionSpec withEngine: anEngine ]
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> label [

	^self versionSpec label
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> name [

	^self versionSpec name
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> operator [

	^self target operator
]

{ #category : 'spec compatibility' }
MetacelloMCVersionSpecLoader >> packageSpecsInLoadOrder [
    ^ self versionSpec packageSpecsInLoadOrderForMap: packages
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> packages [

	^packages
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> project [

	^self versionSpec project
]

{ #category : 'loading' }
MetacelloMCVersionSpecLoader >> repositories [

	^self repositorySpecs
]

{ #category : 'spec compatibility' }
MetacelloMCVersionSpecLoader >> repositorySpecs [

	| repositoryMap |
	repositoryMap := self versionSpec repositories
		                 ifNotNil: [ self versionSpec repositories map ]
		                 ifNil: [ Dictionary new ].
	^ repositoryMap values
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> required [

	^ required ifNil: [ #(  ) ]
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> required: anObject [
	required := anObject
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> target [
1halt.
	^ target ifNil: [
		  target := self versionSpec loader copy.
		  target spec: self.
		  target ]
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> versionSpec [
	^ versionSpec
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> versionSpec: anObject [
	versionSpec := anObject
]

{ #category : 'spec compatibility' }
MetacelloMCVersionSpecLoader >> versionString [

	^self versionSpec versionString
]
