tests
testTempFileFor
	"This should be enough to ensure that https://github.com/dalehenrich/metacello-work/issues/405 doesn't happen again"
	| tempFilePath |

	true ifTrue: [ ^ self skip ]. "This test is bad. It should not be testing tmp location 
	(which are different in different platforms) and in fact, it does not matters a lot, as 
	long as it is a place to put temporal files"

	tempFilePath := MetacelloPlatform current tempFileFor: 'xxx' suffix: '.test'.
	self assert: (tempFilePath beginsWith: '/tmp')