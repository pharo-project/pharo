"
Infos about a package
"
Class {
	#name : #MBPackageInfo,
	#superclass : #MBAbstractPackageInfo,
	#category : #'Versionner-Core-Model'
}

{ #category : #accessing }
MBPackageInfo class >> helpLabel [

	^'Package'
]

{ #category : #accessing }
MBPackageInfo >> basePackageName [
	^ self packageName copyUpToLast: #-
]

{ #category : #converting }
MBPackageInfo >> buildStringOrText [
	| string wc loadedPkgName attributes |
	string := super buildStringOrText.
	(wc := self workingCopy) isNil
		ifTrue: [ ^ string ].
	attributes := OrderedCollection new.
	wc ancestry ancestors notEmpty
		ifTrue: [ 
			loadedPkgName := wc ancestry ancestors first name.
			loadedPkgName = self name
				ifFalse: [ 
					(self compareWorkingCopyNamed: loadedPkgName using: #<=)
						ifTrue: [ attributes add: TextColor red ].
					attributes add: TextEmphasis bold ]].
	self isDirty
		ifTrue: [ attributes add: TextEmphasis underlined ].
	attributes notEmpty 
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string
]

{ #category : #accessing }
MBPackageInfo >> classForBrowsing [

	^self workingCopy packageInfo classes anyOne
]

{ #category : #commands }
MBPackageInfo >> cmdBrowsePackageHistory [
	<command>
	^ MBPackageHistoryCommand
	
]

{ #category : #'as yet unclassified' }
MBPackageInfo >> cmdLoad [
	<command>
	^ MBLoadPackageCommand.
]

{ #category : #commands }
MBPackageInfo >> cmdRevertPackage [
	<command>
	^ MBRevertPackageCommand
]

{ #category : #commands }
MBPackageInfo >> cmdSavePackage [
	<command>
	^ MBSavePackageCommand
]

{ #category : #testing }
MBPackageInfo >> compareWorkingCopyNamed: wcName using: comarisonOperator [
	| fileRef wcRef |
	fileRef := GoferResolvedReference name: self spec file.
	wcRef := GoferResolvedReference name: wcName.
	^ wcRef compare: fileRef using: comarisonOperator
]

{ #category : #testing }
MBPackageInfo >> currentVersionMismatch [
           | wc loadedPkgName |
           (wc := self workingCopy) isNil
                   ifTrue: [ ^ false ].
           wc ancestry ancestors isEmpty
                   ifTrue: [ ^ false ].
           loadedPkgName := wc ancestry ancestors first name.
           loadedPkgName = self name
                   ifTrue: [ ^false ].
            ^ true

]

{ #category : #testing }
MBPackageInfo >> interestedInPackageNamed: aString [

	^aString beginsWith: self packageName
]

{ #category : #'as yet unclassified' }
MBPackageInfo >> mbPacakgeName [
	| string wc loadedPkgName attributes |
	string := ''.
	(wc := self workingCopy) isNil
		ifTrue: [ ^ string ].
	attributes := OrderedCollection new.
	wc ancestry ancestors notEmpty
		ifTrue: [ 
			loadedPkgName := wc ancestry ancestors first name.
			loadedPkgName = self name
				ifFalse: [ 
					(self compareWorkingCopyNamed: loadedPkgName using: #<=)
						ifTrue: [ attributes add: TextColor red ].
					attributes add: TextEmphasis bold ]].
	self isDirty
		ifTrue: [ attributes add: TextEmphasis underlined ].
	attributes notEmpty 
		ifTrue: [ ^ Text string: string attributes: attributes ].
	^ string
]

{ #category : #printing }
MBPackageInfo >> printOn: aStream [
	| wc |
	self isDirty
		ifTrue: [ aStream nextPutAll: '* ' ].
	(wc := self workingCopy) isNil
		ifTrue: [ 
			aStream
				nextPutAll: self packageName;
				nextPutAll: ' ---' ]
		ifFalse: [ 
			wc ancestry ancestors notEmpty
				ifTrue: [ 
					| loadedPkgName refString |
					loadedPkgName := wc ancestry ancestors first name.
					(refString := self extractPackageSignatureFrom: wc) notEmpty
						ifTrue: [ refString := '[' , refString , ']' ].
					loadedPkgName = self name
						ifTrue: [ aStream nextPutAll: self name ]
						ifFalse: [ 
							(self compareWorkingCopyNamed: loadedPkgName using: #<=)
								ifTrue: [ refString := Text string: refString attributes: TextColor red ].
							aStream
								nextPutAll: self name;
								nextPutAll: refString ] ] ]
]

{ #category : #utils }
MBPackageInfo >> repositories [
	"Return the repositories associated with the package."

	^self spec workingCopy repositoryGroup repositories
]

{ #category : #accessing }
MBPackageInfo >> typeLabel [

	^'package'
]

{ #category : #'as yet unclassified' }
MBPackageInfo >> versions [
	""
	|wc v|
	(wc := self workingCopy) isNil
                   ifTrue: [ ^ #() ].
      ^ (wc ancestry breadthFirstAncestors) collect: [ : vi| vi].
]
