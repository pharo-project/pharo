Class {
	#name : #MetacelloRepositorySqueakCommonTestCase,
	#superclass : #AbstractEnvironmentTestCase,
	#category : #'Metacello-TestsPlatform'
}

{ #category : #baselines }
MetacelloRepositorySqueakCommonTestCase >> baseline10: spec [
    <version: '1.0-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: self directoryForPlatform ] ]
]

{ #category : #baselines }
MetacelloRepositorySqueakCommonTestCase >> baseline13: spec [
    <version: '1.3-baseline'>
    spec
        for: #'squeakCommon'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: 'ftp://ftp.example.com/examples' ] ]
]

{ #category : #baselines }
MetacelloRepositorySqueakCommonTestCase >> baseline14: spec [
    <version: '1.4-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: 'http://example.com/examples' ] ]
]

{ #category : #baselines }
MetacelloRepositorySqueakCommonTestCase >> baseline15: spec [
    <version: '1.5-baseline'>
    spec
        for: #'common'
        do: [ 
            spec
                package: 'Example-Core'
                with: [ 
                    spec
                        includes: 'Example-AddOn';
                        file: 'Example-Core-anon.1';
                        repository: 'dictionary://Metacello_Platform_Test_GlobalDictionary' ] ]
]

{ #category : #baselines }
MetacelloRepositorySqueakCommonTestCase >> baseline16: spec [
  <version: '1.6-baseline'>
  spec
    for: #'common'
    do: [ 
      spec
        package: 'Example-Core'
        with: [ 
          spec
            includes: 'Example-AddOn';
            file: 'Example-Core-anon.1';
            repository:
                'filetree://' , MCFileTreeFileUtils current default fullName , '/temp/repo' ] ]


]

{ #category : #accessing }
MetacelloRepositorySqueakCommonTestCase >> directoryForPlatform [ 
	| dir |
	"I don't know how to do this in other platforms (not Pharo)"
	dir := '/opt/mcexamples'.
	(Smalltalk classNamed: #OSPlatform) ifNotNil: [ :c | 
		c current isWindows
			ifTrue: [ dir := 'c:\opt\mcexamples' ] ].
	^ dir
]

{ #category : #accessing }
MetacelloRepositorySqueakCommonTestCase >> project [
     "force new version as workaround for https://code.google.com/p/smalltalk-hub/issues/detail?id=21"

	| constructor project |
	"Construct Metacello project"
	constructor := MetacelloVersionConstructor on: self.
	project := constructor project.
	project loader: MetacelloNullRecordingMCSpecLoader new.
	^project
]

{ #category : #running }
MetacelloRepositorySqueakCommonTestCase >> setUp [
    super setUp.
    testingEnvironment
        at: #'Metacello_Platform_Test_GlobalDictionary'
        put:
            (MCDictionaryRepository new
                description: 'dictionary://Metacello_Platform_Test_GlobalDictionary';
                yourself)
]

{ #category : #running }
MetacelloRepositorySqueakCommonTestCase >> tearDown [
    
    testingEnvironment removeKey: #'Metacello_Platform_Test_GlobalDictionary' ifAbsent: [  ].
	super tearDown
]

{ #category : #tests }
MetacelloRepositorySqueakCommonTestCase >> testAsRepositorySpecFor [
	| project testBlock |
	project := self project.
	testBlock := [ :pkg :expectedType1 :expectedType2 | 
	| repoSpec1 repo repoSpec2 |
	repoSpec1 := pkg repositorySpecs first.
	self assert: repoSpec1 type equals: expectedType1.
	repo := repoSpec1 createRepository.
	repoSpec2 := repo asRepositorySpecFor: project.
	self assert: repoSpec2 type equals: expectedType2.
	expectedType1 = expectedType2
		ifTrue: [ self assert: repoSpec1 description equals: repoSpec2 description ]
		ifFalse:
			[ "special case for standard directory type that defaults to MCServerDirectoryRepository" self assert: (repoSpec1 description copyFrom: 'server://' size + 1 to: repoSpec1 description size) equals: repoSpec2 description ] ].
	testBlock value: ((project version: '1.0-baseline') packageNamed: 'Example-Core') value: 'directory' value: 'directory'.
	testBlock value: ((project version: '1.3-baseline') packageNamed: 'Example-Core') value: 'ftp' value: 'ftp'.
	testBlock value: ((project version: '1.4-baseline') packageNamed: 'Example-Core') value: 'http' value: 'http'.
	testBlock value: ((project version: '1.5-baseline') packageNamed: 'Example-Core') value: 'dictionary' value: 'dictionary'
]

{ #category : #tests }
MetacelloRepositorySqueakCommonTestCase >> testDictionaryRepository [
	| project pkg repoSpec repo |
	project := self project.
	pkg := (project version: '1.5-baseline') packageNamed: 'Example-Core'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'dictionary'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCDictionaryRepository.
	self assert: repo dictionary identicalTo: (testingEnvironment at: #Metacello_Platform_Test_GlobalDictionary) dictionary
]

{ #category : #tests }
MetacelloRepositorySqueakCommonTestCase >> testDirectoryRepository [
	| project pkg repoSpec repo |
	project := self project.
	pkg := (project version: '1.0-baseline') packageNamed: 'Example-Core'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'directory'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCDirectoryRepository.
	self assert: repo description equals: self directoryForPlatform
]

{ #category : #tests }
MetacelloRepositorySqueakCommonTestCase >> testFileTreeRepository [
	"must create repository against an existing directory"

	| project pkg repoSpec repo fileUtils workingDir tempDir repoDir |
	fileUtils := MCFileTreeFileUtils current.
	workingDir := fileUtils default.
	tempDir := fileUtils directoryFromPath: 'temp' relativeTo: workingDir.
	[ fileUtils ensureDirectoryExists: tempDir.
	repoDir := fileUtils directoryFromPath: 'repo' relativeTo: tempDir.
	fileUtils ensureDirectoryExists: repoDir.
	project := self project.
	pkg := (project version: '1.6-baseline') packageNamed: 'Example-Core'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'filetree'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: (testingEnvironment at: #MCFileTreeRepository ifAbsent: [  ]).
	MetacelloNotification signal: ('testFileTreeRepository: ' , (fileUtils directoryFromPath: 'temp/repo' relativeTo: fileUtils default) fullName).
	self assert: repo description equals: 'filetree://' , (fileUtils directoryFromPath: 'temp/repo' relativeTo: fileUtils default) fullName ]
		ensure: [ (fileUtils directoryExists: tempDir) ifTrue: [ fileUtils deleteAll: tempDir ] ]
]

{ #category : #tests }
MetacelloRepositorySqueakCommonTestCase >> testFtpRepository [
	| project pkg repoSpec repo |
	project := self project.
	pkg := (project version: '1.3-baseline') packageNamed: 'Example-Core'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'ftp'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCFtpRepository.
	self assert: repo host equals: 'ftp.example.com'.
	self assert: repo directory equals: 'examples'
]

{ #category : #tests }
MetacelloRepositorySqueakCommonTestCase >> testHttpRepository [
	| project pkg repoSpec repo |
	project := self project.
	pkg := (project version: '1.4-baseline') packageNamed: 'Example-Core'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'http'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCHttpRepository.
	self assert: repo description equals: 'http://example.com/examples'
]
