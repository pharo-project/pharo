Class {
	#name : 'MetacelloRepositoryCommonTestCase',
	#superclass : 'AbstractEnvironmentTestCase',
	#category : 'Metacello-TestsMCCore',
	#package : 'Metacello-TestsMCCore'
}

{ #category : 'baselines' }
MetacelloRepositoryCommonTestCase >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec package: 'Example-Core' with: [
					spec
						includes: 'Example-AddOn';
						file: 'Example-Core-anon.1';
						repository: (OSPlatform current isWindows
								 ifTrue: [ 'c:\opt\mcexamples' ]
								 ifFalse: [ '/opt/mcexamples' ]) ].

			spec package: 'Example-Core2' with: [
					spec
						includes: 'Example-AddOn2';
						file: 'Example-Core2-anon.1';
						repository: 'ftp://ftp.example.com/examples' ].

			spec package: 'Example-Core3' with: [
					spec
						includes: 'Example-AddOn3';
						file: 'Example-Core3-anon.1';
						repository: 'http://example.com/examples' ].
					
			spec package: 'Example-Core4' with: [
					spec
						includes: 'Example-AddOn4';
						file: 'Example-Core4-anon.1';
						repository: 'dictionary://Metacello_Platform_Test_GlobalDictionary' ].

			spec package: 'Example-Core5' with: [
					spec
						includes: 'Example-AddOn5';
						file: 'Example-Core5-anon.1';
						repository: 'filetree://' , MCFileTreeFileUtils default fullName , '/temp/repo' ] ]
]

{ #category : 'accessing' }
MetacelloRepositoryCommonTestCase >> project [

	^ (MetacelloBaselineConstructor on: self) project
]

{ #category : 'running' }
MetacelloRepositoryCommonTestCase >> setUp [
    super setUp.
    testingEnvironment
        at: #'Metacello_Platform_Test_GlobalDictionary'
        put:
            (MCDictionaryRepository new
                description: 'dictionary://Metacello_Platform_Test_GlobalDictionary';
                yourself)
]

{ #category : 'running' }
MetacelloRepositoryCommonTestCase >> tearDown [

	testingEnvironment removeKey: #Metacello_Platform_Test_GlobalDictionary ifAbsent: [  ].
	super tearDown
]

{ #category : 'tests' }
MetacelloRepositoryCommonTestCase >> testAsRepositorySpecFor [

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
			             ifFalse: [ "special case for standard directory type that defaults to MCServerDirectoryRepository"
				             self assert: (repoSpec1 description copyFrom: 'server://' size + 1 to: repoSpec1 description size) equals: repoSpec2 description ] ].
	testBlock value: (project version packageNamed: 'Example-Core') value: 'directory' value: 'directory'.
	testBlock value: (project version packageNamed: 'Example-Core2') value: 'ftp' value: 'ftp'.
	testBlock value: (project version packageNamed: 'Example-Core3') value: 'http' value: 'http'.
	testBlock value: (project version packageNamed: 'Example-Core4') value: 'dictionary' value: 'dictionary'
]

{ #category : 'tests' }
MetacelloRepositoryCommonTestCase >> testDictionaryRepository [

	| pkg repoSpec repo |
	pkg := self project version packageNamed: 'Example-Core4'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'dictionary'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCDictionaryRepository.
	self assert: repo dictionary identicalTo: (testingEnvironment at: #Metacello_Platform_Test_GlobalDictionary) dictionary
]

{ #category : 'tests' }
MetacelloRepositoryCommonTestCase >> testDirectoryRepository [

	| pkg repoSpec repo |
	pkg := self project version packageNamed: 'Example-Core'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'directory'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCDirectoryRepository.
	self assert: repo description equals: (OSPlatform current isWindows
			 ifTrue: [ 'c:\opt\mcexamples' ]
			 ifFalse: [ '/opt/mcexamples' ])
]

{ #category : 'tests' }
MetacelloRepositoryCommonTestCase >> testFileTreeRepository [
	"must create repository against an existing directory"

	| pkg repoSpec repo fileUtils workingDir tempDir repoDir |
	fileUtils := MCFileTreeFileUtils.
	workingDir := fileUtils default.
	tempDir := fileUtils directoryFromPath: 'temp' relativeTo: workingDir.
	[
		fileUtils ensureDirectoryExists: tempDir.
		repoDir := fileUtils directoryFromPath: 'repo' relativeTo: tempDir.
		fileUtils ensureDirectoryExists: repoDir.
		pkg := self project version packageNamed: 'Example-Core5'.
		repoSpec := pkg repositorySpecs first.
		self assert: repoSpec type equals: 'filetree'.
		repo := repoSpec createRepository.
		self assert: repo class identicalTo: (testingEnvironment at: #MCFileTreeRepository ifAbsent: [ ]).
		MetacelloNotification signal: 'testFileTreeRepository: ' , (fileUtils directoryFromPath: 'temp/repo' relativeTo: fileUtils default) fullName.
		self assert: repo description equals: 'filetree://' , (fileUtils directoryFromPath: 'temp/repo' relativeTo: fileUtils default) fullName ] ensure: [
		(fileUtils directoryExists: tempDir) ifTrue: [ fileUtils deleteAll: tempDir ] ]
]

{ #category : 'tests' }
MetacelloRepositoryCommonTestCase >> testFtpRepository [

	| pkg repoSpec repo |
	pkg := self project version packageNamed: 'Example-Core2'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'ftp'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCFtpRepository.
	self assert: repo host equals: 'ftp.example.com'.
	self assert: repo directory equals: 'examples'
]

{ #category : 'tests' }
MetacelloRepositoryCommonTestCase >> testHttpRepository [

	| pkg repoSpec repo |
	pkg := self project version packageNamed: 'Example-Core3'.
	repoSpec := pkg repositorySpecs first.
	self assert: repoSpec type equals: 'http'.
	repo := repoSpec createRepository.
	self assert: repo class identicalTo: MCHttpRepository.
	self assert: repo description equals: 'http://example.com/examples'
]
