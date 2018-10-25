Class {
	#name : #MCGitlabRepositoryTest,
	#superclass : #TestCase,
	#instVars : [
		'hostname'
	],
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
		assert: repository repoPath equals: 'src'.

	repository := MCGitlabRepository location: 'gitlab://pharo-project/pharo:master'.
	self
		assert: repository hostname equals: 'gitlab.com';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath isEmpty.

	repository := MCGitlabRepository location: 'gitlab://pharo-project/pharo'.
	self
		assert: repository hostname equals: 'gitlab.com';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath isEmpty
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
		assert: repository repoPath equals: 'src'.

	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:pharo-project/pharo:master'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath isEmpty.

	repository := MCGitlabRepository location: 'gitlab://git.pharo.org:pharo-project/pharo'.
	self
		assert: repository hostname equals: 'git.pharo.org';
		assert: repository projectPath equals: 'pharo-project/pharo';
		assert: repository projectVersion equals: 'master';
		assert: repository repoPath isEmpty
]
