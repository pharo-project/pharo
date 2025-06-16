Class {
	#name : 'MCSmalltalkhubRepositoryTest',
	#superclass : 'MCTestCase',
	#category : 'Monticello-Tests-Repository',
	#package : 'Monticello-Tests',
	#tag : 'Repository'
}

{ #category : 'tests' }
MCSmalltalkhubRepositoryTest >> testVersionFromFileNamed [

	| repository version |

	"I am accessing the internet, give me more time"
	self timeLimit: 15 seconds.

	repository := MCSmalltalkhubRepository owner: 'Pharo' project: 'Pharo10'.
	version := repository versionFromFileNamed: 'Kernel-sd.152.mcz'.
	self assert: (version info hasID: (UUID fromString: '085d31bf-e12f-4f0e-8b3f-a7aae927c7b8')).
]
