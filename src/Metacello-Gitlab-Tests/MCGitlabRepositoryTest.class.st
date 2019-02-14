Class {
	#name : #MCGitlabRepositoryTest,
	#superclass : #TestCase,
	#category : #'Metacello-Gitlab-Tests'
}

{ #category : #tests }
MCGitlabRepositoryTest >> testLocation [
	| repository |
	repository := MCGitlabRepository location: 'gitlab://pharo-project/pharo:master/src'.
	self
		assert: repository hostname equals: 'gitlab.com';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath equals: 'src';
		assert: repository sshPort isNil.

	repository := MCGitlabRepository location: 'gitlab://pharo-project/pharo:master'.
	self
		assert: repository hostname equals: 'gitlab.com';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assertEmpty: repository repoPath;
		assert: repository sshPort isNil.

	repository := MCGitlabRepository location: 'gitlab://pharo-project/pharo'.
	self
		assert: repository hostname equals: 'gitlab.com';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assertEmpty: repository repoPath;
		assert: repository sshPort isNil
]

{ #category : #tests }
MCGitlabRepositoryTest >> testProjectTagsUrlFor [
	| repository |
	repository := MCGitlabRepository location: 'gitlab://pharo-project/pharo:master/src'.
	self assert: (repository projectTagsUrlFor: 'pharo-project/pharo') equals: 'https://gitlab.com/api/v4/projects/pharo-project/pharo/repository/tags'.

	"Self-hosted instance"
	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:pharo-project/pharo:master/src'.
	self assert: (repository projectTagsUrlFor: 'pharo-project/pharo') equals: 'https://git.pharo.org/api/v4/projects/pharo-project/pharo/repository/tags'
]

{ #category : #tests }
MCGitlabRepositoryTest >> testSelfHostedLocation [
	| repository |
	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:pharo-project/pharo:master/src'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath equals: 'src';
		assert: repository sshPort isNil.

	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:pharo-project/pharo:master'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assertEmpty: repository repoPath;
		assert: repository sshPort isNil.

	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:pharo-project/pharo'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assertEmpty: repository repoPath;
		assert: repository sshPort isNil
]

{ #category : #tests }
MCGitlabRepositoryTest >> testSelfHostedLocationWithNonDefaultSSHPort [
	| repository |
	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:1234:pharo-project/pharo:master/src'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath equals: 'src';
		assert: repository sshPort equals: '1234'.

	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:1234:pharo-project/pharo:master'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assertEmpty: repository repoPath;
		assert: repository sshPort equals: '1234'.

	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:1234:pharo-project/pharo'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assertEmpty: repository repoPath;
		assert: repository sshPort equals: '1234'
]
