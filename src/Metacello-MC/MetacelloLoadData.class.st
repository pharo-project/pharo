Class {
	#name : #MetacelloLoadData,
	#superclass : #Object,
	#instVars : [
		'dataMap',
		'versionInfoMap',
		'packageNameMap'
	],
	#category : #'Metacello-MC-Loaders'
}

{ #category : #accessing }
MetacelloLoadData >> addVersion: version versionInfo: versionInfo resolvedReference: resolvedReference packageSpec: packageSpec [
	| vis |
	"check for duplicates and use the one that is being added"
	(vis := self packageNameMap at: packageSpec name ifAbsent: [  ]) ~~ nil
		ifTrue: [ 
			"remove old references"
			vis
				do: [ :vi | 
					self dataMap removeKey: vi name.
					self versionInfoMap removeKey: vi name ] ].
	self dataMap
		at: version info name
		put:
			{version.
			resolvedReference.
			packageSpec}.
	self versionInfoMap at: versionInfo name put: {versionInfo}.
	self packageNameMap at: packageSpec name put: {versionInfo}
]

{ #category : #versionInfo }
MetacelloLoadData >> ancestorsFor: packageSpec ifAbsent: aBlock [

	^self versionInfoMap 
		at: packageSpec file 
		ifAbsent: [ 
			self  packageNameMap 
				at: packageSpec name
				ifAbsent: aBlock ]
]

{ #category : #versionInfo }
MetacelloLoadData >> currentVersionInfoFor: packageSpec ifAbsent: aBlock [

	^self versionInfoMap 
		at: packageSpec file 
		ifPresent: [ :v | v first ]
		ifAbsent: [ 
			self  packageNameMap 
				at: packageSpec name
				ifPresent: [ :v | v first ]
				ifAbsent: aBlock ]
]

{ #category : #accessing }
MetacelloLoadData >> dataMap [

	dataMap == nil ifTrue: [ dataMap := Dictionary new ].
	^dataMap
]

{ #category : #enumerating }
MetacelloLoadData >> do: aBlock [

	self dataMap valuesDo: [:ar |
		aBlock value: (ar at: 1) value: (ar at: 2) value: (ar at: 3) ]
]

{ #category : #testing }
MetacelloLoadData >> isEmpty [

	^self dataMap isEmpty
]

{ #category : #accessing }
MetacelloLoadData >> packageNameMap [

	packageNameMap == nil ifTrue: [ packageNameMap := Dictionary new ].
	^packageNameMap
]

{ #category : #accessing }
MetacelloLoadData >> versionInfoMap [

	versionInfoMap == nil ifTrue: [ versionInfoMap := Dictionary new ].
	^versionInfoMap
]
