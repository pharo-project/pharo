Class {
	#name : 'MetacelloMCVersionSpecLoader',
	#superclass : 'Object',
	#instVars : [
		'versionSpec',
		'required',
		'packages',
		'loader',
		'loaderPolicy'
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

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> label [

	^self versionSpec label
]

{ #category : 'loading' }
MetacelloMCVersionSpecLoader >> loadWithEngine: anEngine [

	| target |
	packages := Dictionary new.
	self versionSpec resolveToLoadableSpecs: required map: packages.
	target := self target.
	packages values do: [ :pkg |
		pkg ensureLoadedForDevelopmentUsing: target withEngine: anEngine.
		(MetacelloIgnorePackageLoaded signal: pkg) ifFalse: [
			anEngine ignoreImage ifFalse: [
				(pkg compareCurrentVersion: self operator targetVersionStatus: #( #allLoadedToSpec ) using: target withEngine: anEngine) ifTrue: [ packages removeKey: pkg name ] ] ] ].
	packages notEmpty ifTrue: [
		target preLoad: self versionSpec withEngine: anEngine.
		target load: anEngine.
		target postLoad: self versionSpec withEngine: anEngine ].
	^ target
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> loaderPolicy [
	^ loaderPolicy
]

{ #category : 'accessing' }
MetacelloMCVersionSpecLoader >> loaderPolicy: anObject [
	loaderPolicy := anObject
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

	^ loader ifNil: [
		  loader := self versionSpec loader copy.
		  loader spec: self.
		  loaderPolicy ifNotNil: [ loader loaderPolicy: loaderPolicy ].
		  loader ]
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
