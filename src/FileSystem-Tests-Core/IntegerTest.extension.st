Extension { #name : #IntegerTest }

{ #category : #'*FileSystem-Tests-Core' }
IntegerTest >> testHumanReadableByteSizeString [
	self assert: 1000 humanReadableByteSizeString equals: '1000 B'.
	self assert: 1024 humanReadableByteSizeString equals: '1 KB'.

	self assert: (1000 * 1000) humanReadableByteSizeString equals: '976 KB'.
	self assert: (1024 * 1024) humanReadableByteSizeString equals: '1 MB'.

	self assert: (1000 * 1000 * 1000) humanReadableByteSizeString equals: '953 MB'.
	self assert: (1024 * 1024 * 1024) humanReadableByteSizeString equals: '1 GB'.

	self assert: (1000 * 1000 * 1000 * 1000) humanReadableByteSizeString equals: '931 GB'.
	self assert: (1024 * 1024 * 1024 * 1024) humanReadableByteSizeString equals: '1 TB'
]
