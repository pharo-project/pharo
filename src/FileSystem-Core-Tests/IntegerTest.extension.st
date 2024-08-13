Extension { #name : 'IntegerTest' }

{ #category : '*FileSystem-Core-Tests' }
IntegerTest >> testHumanReadableByteSizeString [
	self assert: 1000 humanReadableByteSizeString equals: '1000.00 B'.
	self assert: 1024 humanReadableByteSizeString equals: '1.00 KB'.

	self assert: (1000 * 1000) humanReadableByteSizeString equals: '976.56 KB'.
	self assert: (1024 * 1024) humanReadableByteSizeString equals: '1.00 MB'.
	self assert: (1.25 * 1024 * 1024) asInteger humanReadableByteSizeString equals: '1.25 MB'.

	self assert: (1000 * 1000 * 1000) humanReadableByteSizeString equals: '953.67 MB'.
	self assert: (1024 * 1024 * 1024) humanReadableByteSizeString equals: '1.00 GB'.
	self assert: (2.36 * 1024 * 1024 * 1024) asInteger humanReadableByteSizeString equals: '2.36 GB'.

	self assert: (1000 * 1000 * 1000 * 1000) humanReadableByteSizeString equals: '931.32 GB'.
	self assert: (1024 * 1024 * 1024 * 1024) humanReadableByteSizeString equals: '1.00 TB'.
	self assert: (4.56 * 1024 * 1024 * 1024 * 1024) asInteger humanReadableByteSizeString equals: '4.56 TB'
]
