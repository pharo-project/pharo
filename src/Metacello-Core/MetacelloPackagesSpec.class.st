Class {
	#name : 'MetacelloPackagesSpec',
	#superclass : 'MetacelloSpec',
	#instVars : [
		'list',
		'memberMap'
	],
	#category : 'Metacello-Core-Model',
	#package : 'Metacello-Core',
	#tag : 'Model'
}

{ #category : 'class initialization' }
MetacelloPackagesSpec class >> initialize [ 
	self deprecatedAliases: #(MetacelloMemberListSpec)
]

{ #category : 'actions' }
MetacelloPackagesSpec >> add: aSpec [

	self addMember: (MetacelloAddMemberSpec spec: aSpec)
]

{ #category : 'adding' }
MetacelloPackagesSpec >> addMember: aMember [

	self list add: aMember.
	self clearMemberMap
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> applyIncludesTo: orderedSpecs for: pkgSpec firstTime: firstTime for: aVersionSpec [

	| movedSpecs baseIndex |
	movedSpecs := Set new.
	baseIndex := orderedSpecs indexOf: pkgSpec.
	(pkgSpec includeSpecNamesForPackageOrdering: aVersionSpec) do: [ :includedSpec |
		| includedSpecName spec |
		includedSpecName := includedSpec name.
		spec := orderedSpecs
			        detect: [ :aSpec | aSpec name = includedSpecName ]
			        ifNone: [  ].
		(self
			 slideIn: orderedSpecs
			 spec: spec
			 baseIndex: baseIndex
			 seen: IdentitySet new
			 firstTime: firstTime
			 for: aVersionSpec) ifTrue: [ movedSpecs add: spec name ] ].
	^ movedSpecs
]

{ #category : 'private' }
MetacelloPackagesSpec >> clearMemberMap [

	memberMap := nil.
]

{ #category : 'actions' }
MetacelloPackagesSpec >> copy: specNamed to: spec [

	self addMember: ((MetacelloCopyMemberSpec spec: spec)
			 sourceName: specNamed;
			 yourself)
]

{ #category : 'testing' }
MetacelloPackagesSpec >> isEmpty [

	^ self loadableSpecs isEmpty
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> list [

	^ list ifNil: [ list := OrderedCollection new ]
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> list: aCollection [

	list := aCollection.
	self clearMemberMap
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> loadableSpecNamed: aString ifAbsent: aBlock [

	^ self loadableSpecs detect: [ :spec | spec name = aString ] ifNone: aBlock
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> loadableSpecNamed: aString ifPresent: aBlock [

	^ self loadableSpecs detect: [ :spec | spec name = aString ] ifFound: aBlock
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> loadableSpecs [
	"Return the list of specs I contains once all members are applied."

	^ self map values
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> map [

	| map |
	memberMap ifNotNil: [ ^ memberMap ].
	map := OrderedDictionary new.
	self list do: [ :member | member applyToMap: map ].
	^ memberMap := map
]

{ #category : 'actions' }
MetacelloPackagesSpec >> merge: aSpec [

	self addMember: (MetacelloMergeMemberSpec spec: aSpec)
]

{ #category : 'merging' }
MetacelloPackagesSpec >> mergeSpec: anotherSpec [

	| newSpec |
	newSpec := super mergeSpec: anotherSpec.
	newSpec list: self list copy.
	anotherSpec list do: [ :groupMember | newSpec addMember: groupMember ].
	^ newSpec
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> packageSpecsInLoadOrderFor: aVersionSpec [
	"order only the packageSpecs and projectSpecs. groupSpec order does not matter"

	| orderedPackageSpecs groupSpecs moved lastMovedSpecs count terminationLimit specsWithIncludes firstTime groupLoops |
	orderedPackageSpecs := OrderedCollection new.
	groupSpecs := Set new.
	self loadableSpecs do: [ :spec |
			(orderedPackageSpecs includes: spec) ifFalse: [
					spec isGroupSpec
						ifTrue: [ groupSpecs add: spec ]
						ifFalse: [ orderedPackageSpecs add: spec ] ] ].
	orderedPackageSpecs ifEmpty: [ ^ orderedPackageSpecs , groupSpecs asOrderedCollection ].
	moved := true.
	count := 0.
	terminationLimit := orderedPackageSpecs size * 2.
	groupLoops := IdentitySet new.
	[ moved ] whileTrue: [
			count := count + 1.
			count > terminationLimit ifTrue: [ "Cheap termination hack - an APPARENT loop" self error: 'Apparent loop in before/after dependency definitions' ].
			moved := false.
			orderedPackageSpecs copy do: [ :packageSpec |
					moved := moved or: [
							         self
								         sortPackageSpecs: orderedPackageSpecs
								         packageSpec: packageSpec
								         groupLoops: groupLoops
								         for: aVersionSpec ] ] ].
	lastMovedSpecs := Set new.
	moved := true.
	count := 0.
	specsWithIncludes := orderedPackageSpecs select: [ :pkgSpec | pkgSpec includesForPackageOrdering isNotEmpty ].
	firstTime := true.
	[ moved ] whileTrue: [
			| result |
			count := count + 1. "count > terminationLimit"
			count > 14 ifTrue: [ "Cheap termination hack - an APPARENT loop" self error: 'Apparent loop in before/after dependency definitions' ].
			moved := false.
			result := Set new.
			specsWithIncludes do: [ :packageSpec |
					result addAll: (self
							 applyIncludesTo: orderedPackageSpecs
							 for: packageSpec
							 firstTime: firstTime
							 for: aVersionSpec) ].
			result size = lastMovedSpecs size
				ifTrue: [ result do: [ :name | (lastMovedSpecs includes: name) ifFalse: [ moved := true ] ] ]
				ifFalse: [ moved := true ].
			lastMovedSpecs := result.
			firstTime := false ].
	^ orderedPackageSpecs , groupSpecs asOrderedCollection
]

{ #category : 'copying' }
MetacelloPackagesSpec >> postCopy [

	super postCopy.
	list := list copy.
	self clearMemberMap
]

{ #category : 'printing' }
MetacelloPackagesSpec >> printOn: aStream [

	super printOn: aStream.
	aStream nextPutAll: ' ['.
	self loadableSpecs
		ifEmpty: [ aStream nextPutAll: 'Empty' ]
		ifNotEmpty: [ :specs | specs do: [ :spec | aStream nextPutAll: spec name ] separatedBy: [ aStream nextPutAll: ', ' ] ].
	aStream nextPutAll: ']'
]

{ #category : 'removing' }
MetacelloPackagesSpec >> remove: aSpec [

	self addMember: (MetacelloRemoveMemberSpec spec: aSpec)
]

{ #category : 'accessing' }
MetacelloPackagesSpec >> slideIn: orderedSpecs spec: targetSpec baseIndex: baseIndex seen: seen firstTime: firstTime for: aVersionSpec [

	| targetIndex requiredSpecNames targetRequires targetRequiresIndexes minIndex baseSpec required |
	(seen includes: targetSpec) ifTrue: [ ^ false ].
	targetIndex := orderedSpecs indexOf: targetSpec.
	baseIndex >= targetIndex ifTrue: [ ^ false ].
	required := false.
	baseSpec := orderedSpecs at: baseIndex.
	baseIndex + 1 to: targetIndex - 1 do: [ :index |
			| spec |
			spec := orderedSpecs at: index.
			((spec requiredSpecNamesForPackageOrdering: aVersionSpec) includes: baseSpec name) ifTrue: [ required := true ] ].
	firstTime ifFalse: [ required ifFalse: [ ^ false ] ].
	requiredSpecNames := targetSpec requiredSpecNamesForPackageOrdering: aVersionSpec.
	targetRequires := orderedSpecs select: [ :spec | requiredSpecNames includes: spec name ].
	targetRequiresIndexes := targetRequires collect: [ :spec | orderedSpecs indexOf: spec ].
	targetRequiresIndexes add: baseIndex.
	minIndex := targetRequiresIndexes max.
	minIndex + 1 < targetIndex
		ifTrue: [
				orderedSpecs remove: targetSpec.
				orderedSpecs add: targetSpec afterIndex: minIndex.
				seen add: targetSpec ]
		ifFalse: [
				^ self
					  slideIn: orderedSpecs
					  spec: (orderedSpecs at: minIndex)
					  baseIndex: 1
					  seen: seen
					  firstTime: firstTime
					  for: aVersionSpec ].
	^ true
]

{ #category : 'private' }
MetacelloPackagesSpec >> sortPackageSpecs: orderedSpecs packageSpec: packageSpec groupLoops: groupLoops for: aVersionSpec [

	| packageIndex moved movePackage targetPackage targetIndex targetSpecNames groupLoopDetected |
	packageIndex := orderedSpecs indexOf: packageSpec.
	moved := movePackage := false.
	targetSpecNames := packageSpec requiredSpecNamesForPackageOrdering: aVersionSpec.
	groupLoopDetected := groupLoops includes: packageSpec.
	groupLoopDetected ifFalse: [
			targetSpecNames do: [ :targetPackageOrProjectName |
					targetPackage := orderedSpecs
						                 detect: [ :each | each name = targetPackageOrProjectName ]
						                 ifNone: [
								                 MetacelloNameNotDefinedError signal:
									                 'project group, or package named: ' , targetPackageOrProjectName printString
									                 , ' not found when used in requires: or includes: field of package: ' , packageSpec name printString , ' of '
									                 , aVersionSpec projectLabel , '.'.
								                 nil "return nil if resumed" ].
					targetIndex := orderedSpecs indexOf: targetPackage.
					(groupLoopDetected not and: [ packageIndex = targetIndex ]) ifTrue: [
							self notify: 'A group loop has been detected. The package: ' , packageSpec name printString
								, ' requires a group that includes itself. If you resume, the group loop will be ignored and details will be written to the system log.'.
							groupLoopDetected := true.
							groupLoops add: packageSpec.
							MetacelloNotification signal: 'Package: ' , packageSpec name printString.
							MetacelloNotification signal: 'Raw Requires:' level: 2.
							packageSpec requires do: [ :each | MetacelloNotification signal: each asString level: 3 ].
							MetacelloNotification signal: 'Package: ' , packageSpec name printString , ' has a group loop:'.
							MetacelloNotification signal: 'Expanded Requires:' level: 2.
							targetSpecNames do: [ :each | MetacelloNotification signal: each asString level: 3 ] ].
					movePackage := movePackage or: [ packageIndex <= targetIndex ].
					false ifTrue: [ "use for debugging non-obvious reference loops"
							packageIndex < targetIndex ifTrue: [
								MetacelloNotification signal: 'Moving ' , targetPackageOrProjectName , ' from ' , targetIndex printString , ' to ' , packageIndex printString ] ] ] ].
	groupLoopDetected ifTrue: [ "old implementation that does not resolve group names"
			targetSpecNames := packageSpec requires.
			targetSpecNames do: [ :targetPackageName |
					targetPackage := orderedSpecs
						                 detect: [ :each | each name = targetPackageName ]
						                 ifNone: [ ].
					targetIndex := orderedSpecs indexOf: targetPackage.
					movePackage := movePackage or: [ packageIndex <= targetIndex ] ] ].
	movePackage ifTrue: [
			moved := true.
			orderedSpecs remove: packageSpec ifAbsent: [ ^ self error: 'unexpected error removing package' ].
			targetIndex := 0.
			targetSpecNames do: [ :targetPackageOrProjectName |
					(targetPackage := orderedSpecs
						                  detect: [ :each | each name = targetPackageOrProjectName ]
						                  ifNone: [ ]) ifNotNil: [ targetIndex := targetIndex max: (orderedSpecs indexOf: targetPackage) ] ].
			targetIndex == 0
				ifTrue: [ orderedSpecs add: packageSpec beforeIndex: packageIndex ]
				ifFalse: [ orderedSpecs add: packageSpec afterIndex: targetIndex ] ].
	^ moved
]
