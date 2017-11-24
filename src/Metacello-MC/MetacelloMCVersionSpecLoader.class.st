Class {
	#name : #MetacelloMCVersionSpecLoader,
	#superclass : #Object,
	#instVars : [
		'versionSpec',
		'required',
		'packages',
		'loader',
		'loaderPolicy'
	],
	#category : #'Metacello-MC-Loaders'
}

{ #category : #'instance creation' }
MetacelloMCVersionSpecLoader class >> on: aVersionSpec [

	^(self new) 
		versionSpec: aVersionSpec;
		yourself
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> label [

	^self versionSpec label
]

{ #category : #loading }
MetacelloMCVersionSpecLoader >> load [
  | mcLoader |
  packages := Dictionary new.
  self resolveToLoadableSpecs.
  mcLoader := self loader.
  packages values
    do: [ :pkg | 
      pkg ensureLoadedForDevelopmentUsing: mcLoader.
      (MetacelloIgnorePackageLoaded signal: pkg)
        ifFalse: [ 
          mcLoader ignoreImage
            ifFalse: [ 
              (pkg
                compareCurrentVersion: self operator
                targetVersionStatus: #(#'allLoadedToSpec')
                using: mcLoader)
                ifTrue: [ packages removeKey: pkg name ] ] ] ].
  packages notEmpty
    ifTrue: [ 
      mcLoader preLoad: self versionSpec.
      mcLoader load.
      mcLoader postLoad: self versionSpec ].
  ^ mcLoader
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> loader [

	loader == nil 
		ifTrue: [
			loader := self versionSpec loader copy.
			loader spec: self.
			loaderPolicy notNil ifTrue: [ loader loaderPolicy: loaderPolicy] ].
	^loader
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> loaderPolicy [
	^ loaderPolicy
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> loaderPolicy: anObject [
	loaderPolicy := anObject
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> name [

	^self versionSpec name
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> operator [

	^self loader operator
]

{ #category : #'spec compatibility' }
MetacelloMCVersionSpecLoader >> packageSpecsInLoadOrder [
    ^ self versionSpec packageSpecsInLoadOrderForMap: packages
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> packages [

	^packages
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> project [

	^self versionSpec project
]

{ #category : #loading }
MetacelloMCVersionSpecLoader >> repositories [

	^self repositorySpecs
]

{ #category : #'spec compatibility' }
MetacelloMCVersionSpecLoader >> repositorySpecs [

	| repositoryMap |
	repositoryMap := self versionSpec repositories ~~ nil
		ifTrue: [ self versionSpec repositories map ]
		ifFalse: [ Dictionary new ].
	^repositoryMap values.
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> required [

	required == nil ifTrue: [ ^#() ].
	^ required
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> required: anObject [
	required := anObject
]

{ #category : #loading }
MetacelloMCVersionSpecLoader >> resolvePackageNames [

	packages := Dictionary new.
	self resolveToLoadableSpecs.
	^packages values collect: [:pkg | pkg name ]
]

{ #category : #private }
MetacelloMCVersionSpecLoader >> resolveToLoadableSpecs [
    self versionSpec resolveToLoadableSpecs: required forLoad: true map: packages
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> versionSpec [
	^ versionSpec
]

{ #category : #accessing }
MetacelloMCVersionSpecLoader >> versionSpec: anObject [
	versionSpec := anObject
]

{ #category : #'spec compatibility' }
MetacelloMCVersionSpecLoader >> versionString [

	^self versionSpec versionString
]
