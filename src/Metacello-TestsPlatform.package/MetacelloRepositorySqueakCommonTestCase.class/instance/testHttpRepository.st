tests
testHttpRepository
    | project pkg repoSpec repo |
    project := self project.
    pkg := (project version: '1.4-baseline') packageNamed: 'Example-Core'.
    repoSpec := pkg repositorySpecs first.
    self assert: repoSpec type = 'http'.
    repo := repoSpec createRepository.
    self assert: repo class == MCHttpRepository.
    self assert: repo description = 'http://example.com/examples'