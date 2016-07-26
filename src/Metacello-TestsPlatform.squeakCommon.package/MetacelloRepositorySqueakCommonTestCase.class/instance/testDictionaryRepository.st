tests
testDictionaryRepository
    | project pkg repoSpec repo |
    project := self project.
    pkg := (project version: '1.5-baseline') packageNamed: 'Example-Core'.
    repoSpec := pkg repositorySpecs first.
    self assert: repoSpec type = 'dictionary'.
    repo := repoSpec createRepository.
    self assert: repo class == MCDictionaryRepository.
    self assert: repo dictionary == (Smalltalk at: #'Metacello_Platform_Test_GlobalDictionary') dictionary