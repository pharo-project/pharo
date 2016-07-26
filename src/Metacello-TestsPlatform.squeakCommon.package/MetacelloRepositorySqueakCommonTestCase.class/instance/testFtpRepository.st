tests
testFtpRepository
    | project pkg repoSpec repo |
    project := self project.
    pkg := (project version: '1.3-baseline') packageNamed: 'Example-Core'.
    repoSpec := pkg repositorySpecs first.
    self assert: repoSpec type = 'ftp'.
    repo := repoSpec createRepository.
    self assert: repo class == MCFtpRepository.
    self assert: repo host = 'ftp.example.com'.
    self assert: repo directory = 'examples'